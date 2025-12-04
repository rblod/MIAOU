!===============================================================================
!> @file io_netcdf.F90
!>
!> @brief NetCDF implementation of the I/O system
!>
!> This module provides the NetCDF-specific implementation of the I/O system.
!> It handles file creation, variable definition, and data writing using the
!> NetCDF library.
!>
!> @author Rachid Benshila
!> @date 2025-04
!===============================================================================
module io_netcdf
   use netcdf
   use io_constants, only: IO_PATH_LEN, IO_FILL_UNSET
   use netcdf_utils, only: nc_check
   use netcdf_backend, only: nc_define_variable, nc_write_variable
   use grid_module, only: grid, axis, create_axis
   use io_definitions, only: io_variable
   use io_file_registry, only: avg_state
   implicit none
   private

   ! Public procedures
   public :: nc_initialize, nc_finalize
   public :: nc_create_file, nc_close_file
   public :: nc_define_variable_in_file
   public :: nc_end_definition
   public :: nc_write_variable_data
   public :: nc_write_avg_data
   public :: nc_write_time
   ! Direct write (no buffer) - for instant outputs
   public :: nc_write_direct_0d, nc_write_direct_1d, nc_write_direct_2d, nc_write_direct_3d

   ! Module state
   logical, private :: is_initialized = .false.

contains

   !---------------------------------------------------------------------------
   !> @brief Initialize the NetCDF I/O system
   !---------------------------------------------------------------------------
   subroutine nc_initialize()
      is_initialized = .true.
   end subroutine nc_initialize

   !---------------------------------------------------------------------------
   !> @brief Finalize the NetCDF I/O system
   !---------------------------------------------------------------------------
   subroutine nc_finalize()
      is_initialized = .false.
   end subroutine nc_finalize

   !---------------------------------------------------------------------------
   !> @brief Create a NetCDF output file
   !>
   !> @param[in]  filename     Path to the file
   !> @param[in]  file_name    Logical file name
   !> @param[in]  freq         Output frequency in seconds
   !> @param[in]  time_units   Time units string
   !> @param[in]  calendar     Calendar type
   !> @param[out] ncid         NetCDF file ID
   !> @param[out] time_dimid   Time dimension ID
   !> @param[out] time_varid   Time variable ID
   !> @return     Status (0 = success)
   !---------------------------------------------------------------------------
   function nc_create_file(filename, file_name, freq, time_units, calendar, &
                           ncid, time_dimid, time_varid) result(status)
      character(len=*), intent(in) :: filename, file_name
      real, intent(in) :: freq
      character(len=*), intent(in) :: time_units, calendar
      integer, intent(out) :: ncid, time_dimid, time_varid
      integer :: status

      integer :: ncstatus
      character(len=32) :: date_str, time_str
      character(len=64) :: datetime_str

      status = -1
      ncid = -1
      time_dimid = -1
      time_varid = -1

      ! Create NetCDF-4 file with compression support
      ncstatus = nf90_create(filename, ior(nf90_clobber, nf90_netcdf4), ncid)
      if (ncstatus /= nf90_noerr) then
         print *, "ERROR: Cannot create file: ", trim(filename)
         print *, "       NetCDF error: ", trim(nf90_strerror(ncstatus))
         return
      end if

      ! Create time dimension (unlimited)
      ncstatus = nf90_def_dim(ncid, "time", nf90_unlimited, time_dimid)
      if (ncstatus /= nf90_noerr) then
         print *, "ERROR: Cannot create time dimension in: ", trim(filename)
         ncstatus = nf90_close(ncid)
         ncid = -1
         return
      end if

      ! Create time variable
      ncstatus = nf90_def_var(ncid, "time", nf90_real, [time_dimid], time_varid)
      if (ncstatus /= nf90_noerr) then
         print *, "WARNING: Cannot create time variable in: ", trim(filename)
         time_varid = -1
      else
         ! Add time attributes
         ncstatus = nf90_put_att(ncid, time_varid, "units", trim(time_units))
         ncstatus = nf90_put_att(ncid, time_varid, "standard_name", "time")
         ncstatus = nf90_put_att(ncid, time_varid, "calendar", trim(calendar))
         ncstatus = nf90_put_att(ncid, time_varid, "axis", "T")
      end if

      ! Add global attributes for CF compliance and traceability
      ncstatus = nf90_put_att(ncid, nf90_global, "Conventions", "CF-1.8")
      ncstatus = nf90_put_att(ncid, nf90_global, "title", "MIAOU ocean model output")
      ncstatus = nf90_put_att(ncid, nf90_global, "institution", "LEGOS/CNRS")
      ncstatus = nf90_put_att(ncid, nf90_global, "source", "MIAOU v0.8.0")
      ncstatus = nf90_put_att(ncid, nf90_global, "file_name", trim(file_name))
      ncstatus = nf90_put_att(ncid, nf90_global, "output_frequency_seconds", freq)
      
      ! Creation timestamp
      call date_and_time(date=date_str, time=time_str)
      write(datetime_str, '(A4,"-",A2,"-",A2,"T",A2,":",A2,":",A2)') &
            date_str(1:4), date_str(5:6), date_str(7:8), &
            time_str(1:2), time_str(3:4), time_str(5:6)
      ncstatus = nf90_put_att(ncid, nf90_global, "history", &
                              trim(datetime_str)//" Created by MIAOU I/O system")

      status = 0
   end function nc_create_file

   !---------------------------------------------------------------------------
   !> @brief Close a NetCDF file
   !---------------------------------------------------------------------------
   function nc_close_file(ncid) result(status)
      integer, intent(in) :: ncid
      integer :: status

      if (ncid > 0) then
         status = nf90_close(ncid)
         if (status /= nf90_noerr) then
            status = -1
         else
            status = 0
         end if
      else
         status = 0
      end if
   end function nc_close_file

   !---------------------------------------------------------------------------
   !> @brief Define a variable in a NetCDF file
   !>
   !> @param[in] ncid        NetCDF file ID
   !> @param[in] time_dimid  Time dimension ID
   !> @param[in] var         Variable to define
   !> @return    Status (0 = success)
   !---------------------------------------------------------------------------
   function nc_define_variable_in_file(ncid, time_dimid, var) result(status)
      integer, intent(in) :: ncid, time_dimid
      type(io_variable), intent(in) :: var
      integer :: status

      integer :: varid
      type(axis) :: dummy_axis(1)
      type(axis), allocatable :: local_axes(:)

      status = -1

      if (ncid <= 0 .or. time_dimid <= 0) return

      ! Special case for scalar variables
      if (var%meta%ndims == 0) then
         dummy_axis(1) = create_axis("dummy", "Dummy dimension", "count", 1)
         call nc_define_variable(ncid, 0, var%meta%name, var%meta%long_name, &
                                 var%meta%units, dummy_axis, time_dimid, varid)
      else
         ! For variables with dimensions
         if (allocated(var%meta%var_grid%axes)) then
            allocate(local_axes(size(var%meta%var_grid%axes)))
            local_axes = var%meta%var_grid%axes

            call nc_define_variable(ncid, var%meta%ndims, var%meta%name, &
                                    var%meta%long_name, var%meta%units, &
                                    local_axes, time_dimid, varid)

            deallocate(local_axes)
         else
            print *, "WARNING: Variable ", trim(var%meta%name), " has no axes defined"
            return
         end if
      end if

      ! Add CF-compliant attributes if set
      call add_cf_attributes(ncid, varid, var)

      status = 0
   end function nc_define_variable_in_file

   !---------------------------------------------------------------------------
   !> @brief Add CF-compliant attributes to a variable
   !>
   !> @param[in] ncid   NetCDF file ID
   !> @param[in] varid  Variable ID
   !> @param[in] var    Variable with metadata
   !---------------------------------------------------------------------------
   subroutine add_cf_attributes(ncid, varid, var)
      integer, intent(in) :: ncid, varid
      type(io_variable), intent(in) :: var

      integer :: stat

      ! Standard name (CF convention)
      if (len_trim(var%meta%standard_name) > 0) then
         stat = nf90_put_att(ncid, varid, "standard_name", trim(var%meta%standard_name))
      end if

      ! Valid range (only if explicitly set)
      if (var%meta%valid_min > -huge(1.0)/2.0) then
         stat = nf90_put_att(ncid, varid, "valid_min", var%meta%valid_min)
      end if
      if (var%meta%valid_max < huge(1.0)/2.0) then
         stat = nf90_put_att(ncid, varid, "valid_max", var%meta%valid_max)
      end if

      ! Fill value (only if explicitly set by user)
      if (var%meta%fill_value > IO_FILL_UNSET) then
         stat = nf90_put_att(ncid, varid, "_FillValue", var%meta%fill_value)
      end if

      ! Coordinates
      if (len_trim(var%meta%coordinates) > 0) then
         stat = nf90_put_att(ncid, varid, "coordinates", trim(var%meta%coordinates))
      end if

   end subroutine add_cf_attributes

   !---------------------------------------------------------------------------
   !> @brief End definition mode
   !---------------------------------------------------------------------------
   function nc_end_definition(ncid) result(status)
      integer, intent(in) :: ncid
      integer :: status

      if (ncid <= 0) then
         status = -1
         return
      end if

      status = nf90_enddef(ncid)
      if (status /= nf90_noerr) then
         print *, "WARNING: Error ending definition mode"
         status = -1
      else
         status = 0
      end if
   end function nc_end_definition

   !---------------------------------------------------------------------------
   !> @brief Write instantaneous variable data
   !>
   !> @param[in] ncid        NetCDF file ID
   !> @param[in] time_index  Current time index
   !> @param[in] var         Variable to write
   !> @return    Status (0 = success)
   !---------------------------------------------------------------------------
   function nc_write_variable_data(ncid, time_index, var) result(status)
      integer, intent(in) :: ncid, time_index
      type(io_variable), intent(in) :: var
      integer :: status

      integer :: varid

      status = -1
      if (ncid <= 0) return

      ! Get variable ID
      if (nf90_inq_varid(ncid, trim(var%meta%name), varid) /= nf90_noerr) then
         print *, "WARNING: Variable ", trim(var%meta%name), " not found in file"
         return
      end if

      ! Write based on dimensionality
      ! Uses buffers if use_buffer is true, otherwise pointers
      select case (var%meta%ndims)
      case (0)
         if (var%data%is_valid(0)) then
            if (var%data%use_buffer) then
               call nc_write_variable(ncid, varid, var%data%buf_scalar, time_index)
            else
               call nc_write_variable(ncid, varid, var%data%scalar, time_index)
            end if
            status = 0
         end if
      case (1)
         if (var%data%is_valid(1)) then
            if (var%data%use_buffer) then
               call nc_write_variable(ncid, varid, var%data%buf_1d, time_index)
            else
               call nc_write_variable(ncid, varid, var%data%d1, time_index)
            end if
            status = 0
         end if
      case (2)
         if (var%data%is_valid(2)) then
            if (var%data%use_buffer) then
               call nc_write_variable(ncid, varid, var%data%buf_2d, time_index)
            else
               call nc_write_variable(ncid, varid, var%data%d2, time_index)
            end if
            status = 0
         end if
      case (3)
         if (var%data%is_valid(3)) then
            if (var%data%use_buffer) then
               call nc_write_variable(ncid, varid, var%data%buf_3d, time_index)
            else
               call nc_write_variable(ncid, varid, var%data%d3, time_index)
            end if
            status = 0
         end if
      end select
   end function nc_write_variable_data

   !---------------------------------------------------------------------------
   !> @brief Write averaged variable data
   !>
   !> @param[in] ncid        NetCDF file ID
   !> @param[in] time_index  Current time index
   !> @param[in] var         Variable metadata (for name, ndims)
   !> @param[in] state       Averaging state with accumulated data
   !> @return    Status (0 = success)
   !---------------------------------------------------------------------------
   function nc_write_avg_data(ncid, time_index, var, state) result(status)
      integer, intent(in) :: ncid, time_index
      type(io_variable), intent(in) :: var
      type(avg_state), intent(in) :: state
      integer :: status

      integer :: varid, shp(3)
      real :: scalar_result
      real, allocatable :: result_1d(:), result_2d(:,:), result_3d(:,:,:)
      logical :: success

      status = -1
      if (ncid <= 0) return
      if (.not. state%is_ready()) return

      ! Get variable ID
      if (nf90_inq_varid(ncid, trim(var%meta%name), varid) /= nf90_noerr) then
         return
      end if

      ! Compute and write based on dimensionality
      ! NOTE: We use state buffers for shape, not var%data (which may be empty)
      select case (var%meta%ndims)
      case (0)
         success = state%compute_scalar(scalar_result)
         if (success) then
            call nc_write_variable(ncid, varid, scalar_result, time_index)
            status = 0
         end if

      case (1)
         if (allocated(state%d1)) then
            shp(1) = size(state%d1)
            allocate(result_1d(shp(1)))
            success = state%compute_1d(result_1d)
            if (success) then
               call nc_write_variable(ncid, varid, result_1d, time_index)
               status = 0
            end if
            deallocate(result_1d)
         end if

      case (2)
         if (allocated(state%d2)) then
            shp(1) = size(state%d2, 1)
            shp(2) = size(state%d2, 2)
            allocate(result_2d(shp(1), shp(2)))
            success = state%compute_2d(result_2d)
            if (success) then
               call nc_write_variable(ncid, varid, result_2d, time_index)
               status = 0
            end if
            deallocate(result_2d)
         end if

      case (3)
         if (allocated(state%d3)) then
            shp(1) = size(state%d3, 1)
            shp(2) = size(state%d3, 2)
            shp(3) = size(state%d3, 3)
            allocate(result_3d(shp(1), shp(2), shp(3)))
            success = state%compute_3d(result_3d)
            if (success) then
               call nc_write_variable(ncid, varid, result_3d, time_index)
               status = 0
            end if
            deallocate(result_3d)
         end if
      end select

   end function nc_write_avg_data

   !---------------------------------------------------------------------------
   !> @brief Write time value
   !>
   !> @param[in] ncid        NetCDF file ID
   !> @param[in] time_varid  Time variable ID
   !> @param[in] time_index  Current time index
   !> @param[in] time_value  Time value to write
   !> @return    Status (0 = success)
   !---------------------------------------------------------------------------
   function nc_write_time(ncid, time_varid, time_index, time_value) result(status)
      integer, intent(in) :: ncid, time_varid, time_index
      real, intent(in) :: time_value
      integer :: status

      integer :: ncstatus
      real :: time_slice(1)

      status = -1
      if (ncid <= 0 .or. time_varid <= 0) return

      time_slice(1) = time_value
      ncstatus = nf90_put_var(ncid, time_varid, time_slice, &
                              start=[time_index], count=[1])

      if (ncstatus /= nf90_noerr) then
         print *, "WARNING: Error writing time value"
         return
      end if

      status = 0
   end function nc_write_time

   !---------------------------------------------------------------------------
   !> @brief Write scalar data directly (no buffer)
   !---------------------------------------------------------------------------
   function nc_write_direct_0d(ncid, var_name, time_index, val) result(status)
      integer, intent(in) :: ncid, time_index
      character(len=*), intent(in) :: var_name
      real, intent(in) :: val
      integer :: status, varid

      status = -1
      if (ncid <= 0) return
      if (nf90_inq_varid(ncid, trim(var_name), varid) /= nf90_noerr) return
      call nc_write_variable(ncid, varid, val, time_index)
      status = 0
   end function nc_write_direct_0d

   !---------------------------------------------------------------------------
   !> @brief Write 1D data directly (no buffer)
   !---------------------------------------------------------------------------
   function nc_write_direct_1d(ncid, var_name, time_index, val) result(status)
      integer, intent(in) :: ncid, time_index
      character(len=*), intent(in) :: var_name
      real, intent(in) :: val(:)
      integer :: status, varid

      status = -1
      if (ncid <= 0) return
      if (nf90_inq_varid(ncid, trim(var_name), varid) /= nf90_noerr) return
      call nc_write_variable(ncid, varid, val, time_index)
      status = 0
   end function nc_write_direct_1d

   !---------------------------------------------------------------------------
   !> @brief Write 2D data directly (no buffer)
   !---------------------------------------------------------------------------
   function nc_write_direct_2d(ncid, var_name, time_index, val) result(status)
      integer, intent(in) :: ncid, time_index
      character(len=*), intent(in) :: var_name
      real, intent(in) :: val(:,:)
      integer :: status, varid

      status = -1
      if (ncid <= 0) return
      if (nf90_inq_varid(ncid, trim(var_name), varid) /= nf90_noerr) return
      call nc_write_variable(ncid, varid, val, time_index)
      status = 0
   end function nc_write_direct_2d

   !---------------------------------------------------------------------------
   !> @brief Write 3D data directly (no buffer)
   !---------------------------------------------------------------------------
   function nc_write_direct_3d(ncid, var_name, time_index, val) result(status)
      integer, intent(in) :: ncid, time_index
      character(len=*), intent(in) :: var_name
      real, intent(in) :: val(:,:,:)
      integer :: status, varid

      status = -1
      if (ncid <= 0) return
      if (nf90_inq_varid(ncid, trim(var_name), varid) /= nf90_noerr) return
      call nc_write_variable(ncid, varid, val, time_index)
      status = 0
   end function nc_write_direct_3d

end module io_netcdf
