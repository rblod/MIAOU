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
   use io_constants, only: IO_PATH_LEN
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

      status = -1
      ncid = -1
      time_dimid = -1
      time_varid = -1

      ! Create NetCDF file
      ncstatus = nf90_create(filename, nf90_clobber, ncid)
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
         ! Add attributes
         ncstatus = nf90_put_att(ncid, time_varid, "units", trim(time_units))
         ncstatus = nf90_put_att(ncid, time_varid, "standard_name", "time")
         ncstatus = nf90_put_att(ncid, time_varid, "calendar", trim(calendar))
      end if

      ! Add global attributes
      ncstatus = nf90_put_att(ncid, nf90_global, "file_name", trim(file_name))
      ncstatus = nf90_put_att(ncid, nf90_global, "output_frequency", freq)

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

      status = 0
   end function nc_define_variable_in_file

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
      select case (var%meta%ndims)
      case (0)
         if (var%data%is_valid(0)) then
            call nc_write_variable(ncid, varid, var%data%scalar, time_index)
            status = 0
         end if
      case (1)
         if (var%data%is_valid(1)) then
            call nc_write_variable(ncid, varid, var%data%d1, time_index)
            status = 0
         end if
      case (2)
         if (var%data%is_valid(2)) then
            call nc_write_variable(ncid, varid, var%data%d2, time_index)
            status = 0
         end if
      case (3)
         if (var%data%is_valid(3)) then
            call nc_write_variable(ncid, varid, var%data%d3, time_index)
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
      select case (var%meta%ndims)
      case (0)
         success = state%compute_scalar(scalar_result)
         if (success) then
            call nc_write_variable(ncid, varid, scalar_result, time_index)
            status = 0
         end if

      case (1)
         if (var%data%is_valid(1)) then
            call var%data%get_shape(1, shp)
            allocate(result_1d(shp(1)))
            success = state%compute_1d(result_1d)
            if (success) then
               call nc_write_variable(ncid, varid, result_1d, time_index)
               status = 0
            end if
            deallocate(result_1d)
         end if

      case (2)
         if (var%data%is_valid(2)) then
            call var%data%get_shape(2, shp)
            allocate(result_2d(shp(1), shp(2)))
            success = state%compute_2d(result_2d)
            if (success) then
               call nc_write_variable(ncid, varid, result_2d, time_index)
               status = 0
            end if
            deallocate(result_2d)
         end if

      case (3)
         if (var%data%is_valid(3)) then
            call var%data%get_shape(3, shp)
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

end module io_netcdf
