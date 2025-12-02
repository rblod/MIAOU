!===============================================================================
!> @file io_netcdf.F90
!>
!> NetCDF implementation of the I/O system
!>
!> This module provides the NetCDF-specific implementation of the I/O system.
!> It handles file creation, variable definition, and data writing using the
!> NetCDF library.
!>
!> REFACTORED: Removed duplicate file registry. All file state is now managed
!> through file_descriptor passed from io_manager.
!>
!> @author Rachid Benshila
!> @date 2025-04-11
!===============================================================================
module io_netcdf
   use netcdf
   use io_constants, only: IO_PATH_LEN, IO_PREFIX_LEN
   use netcdf_utils, only: nc_check
   use netcdf_backend, only: nc_define_variable, nc_write_variable
   use grid_module, only: grid, axis, create_axis
   use io_definitions, only: io_variable, file_descriptor
   use io_config, only: get_output_prefix
   implicit none
   private

   ! Public procedures for NetCDF I/O operations
   public :: nc_initialize, nc_finalize
   public :: nc_create_file, nc_close_file
   public :: nc_write_variable_data, nc_write_time
   public :: nc_define_variable_in_file
   public :: nc_end_definition, get_varid_for_variable

   ! Module state
   logical, private :: is_initialized = .false.

contains

   !---------------------------------------------------------------------------
   ! Initialization and cleanup
   !---------------------------------------------------------------------------

   !> Initialize the NetCDF I/O system
   !>
   !> @return Status code (0 = success)
   function nc_initialize() result(status)
      integer :: status

      is_initialized = .true.
      status = 0
   end function nc_initialize

   !> Finalize the NetCDF I/O system
   !>
   !> Note: File closing is now handled by io_manager calling nc_close_file
   !> for each file. This function just cleans up module state.
   !>
   !> @return Status code (0 = success)
   function nc_finalize() result(status)
      integer :: status

      is_initialized = .false.
      status = 0
   end function nc_finalize

   !---------------------------------------------------------------------------
   ! File operations
   !---------------------------------------------------------------------------

   !> Create a NetCDF output file
   !>
   !> @param[in]     filename     Path to the file
   !> @param[in]     file_type    Type of file ("his", "avg", or "rst")
   !> @param[in]     freq         Output frequency in seconds
   !> @param[in]     time_units   Optional: time units
   !> @param[in]     calendar     Optional: calendar type
   !> @param[inout]  file_desc    File descriptor (populated on success)
   !> @return        Status code (0 = success)
   function nc_create_file(filename, file_type, freq, time_units, calendar, file_desc) result(status)
      character(len=*), intent(in) :: filename, file_type
      real, intent(in) :: freq
      character(len=*), intent(in), optional :: time_units, calendar
      type(file_descriptor), intent(inout) :: file_desc
      integer :: status

      integer :: ncid, time_dimid, time_varid, ncstatus

      status = -1

      ! Initialize file descriptor
      file_desc%filename = filename
      file_desc%type = file_type
      file_desc%freq = freq
      file_desc%time_index = 1
      file_desc%backend_id = -1
      file_desc%time_dimid = -1
      file_desc%time_varid = -1

      ! Create NetCDF file
      ncstatus = nf90_create(filename, nf90_clobber, ncid)
      if (ncstatus /= nf90_noerr) then
         print *, "ERROR: Cannot create file: ", trim(filename)
         print *, "       NetCDF error: ", trim(nf90_strerror(ncstatus))
         return
      end if
      file_desc%backend_id = ncid

      ! Create time dimension (unlimited)
      ncstatus = nf90_def_dim(ncid, "time", nf90_unlimited, time_dimid)
      if (ncstatus /= nf90_noerr) then
         print *, "ERROR: Cannot create time dimension in: ", trim(filename)
         print *, "       NetCDF error: ", trim(nf90_strerror(ncstatus))
         ncstatus = nf90_close(ncid)
         file_desc%backend_id = -1
         return
      end if
      file_desc%time_dimid = time_dimid

      ! Create time variable if units provided
      if (present(time_units)) then
         ncstatus = nf90_def_var(ncid, "time", nf90_real, [time_dimid], time_varid)
         if (ncstatus /= nf90_noerr) then
            print *, "WARNING: Cannot create time variable in: ", trim(filename)
         else
            file_desc%time_varid = time_varid
            
            ! Add attributes
            ncstatus = nf90_put_att(ncid, time_varid, "units", trim(time_units))
            ncstatus = nf90_put_att(ncid, time_varid, "standard_name", "time")
            
            if (present(calendar)) then
               ncstatus = nf90_put_att(ncid, time_varid, "calendar", trim(calendar))
            end if

            print *, "Created time variable in file: ", trim(filename), &
               " with units: ", trim(time_units)
         end if
      else
         print *, "Warning: No time units provided for file: ", trim(filename)
      end if

      status = 0
   end function nc_create_file

   !> Close a NetCDF file
   !>
   !> @param[inout] file_desc  File descriptor of the file to close
   !> @return       Status code (0 = success)
   function nc_close_file(file_desc) result(status)
      type(file_descriptor), intent(inout) :: file_desc
      integer :: status

      status = 0

      if (file_desc%backend_id > 0) then
         status = nf90_close(file_desc%backend_id)
         if (status == nf90_noerr) then
            print *, "Closed file: ", trim(file_desc%filename)
            file_desc%backend_id = -1
            status = 0
         else
            print *, "WARNING: Error closing file: ", trim(file_desc%filename)
            print *, "         NetCDF error: ", trim(nf90_strerror(status))
            status = -1
         end if
      end if
   end function nc_close_file

   !---------------------------------------------------------------------------
   ! Variable operations
   !---------------------------------------------------------------------------

   !> Define a variable in a NetCDF file
   !>
   !> @param[in] file_desc  File descriptor
   !> @param[in] var        Variable to define
   !> @return    Status code (0 = success)
   function nc_define_variable_in_file(file_desc, var) result(status)
      type(file_descriptor), intent(in) :: file_desc
      type(io_variable), intent(in) :: var
      integer :: status

      integer :: ncid, varid
      type(axis) :: dummy_axis(1)
      type(axis), allocatable :: local_axes(:)

      status = -1
      ncid = file_desc%backend_id

      ! Check file validity
      if (ncid <= 0) then
         print *, "ERROR: Invalid NetCDF file ID for: ", trim(file_desc%filename)
         return
      end if

      ! Check time dimension
      if (file_desc%time_dimid <= 0) then
         print *, "WARNING: No time dimension in file: ", trim(file_desc%filename)
         return
      end if

      ! Special case for scalar variables
      if (var%ndims == 0) then
         dummy_axis(1) = create_axis("dummy", "Dummy dimension", "count", 1)
         call nc_define_variable(ncid, 0, var%name, var%long_name, var%units, &
                                 dummy_axis, file_desc%time_dimid, varid)
      else
         ! For variables with dimensions
         if (allocated(var%var_grid%axes)) then
            allocate (local_axes(size(var%var_grid%axes)))
            local_axes = var%var_grid%axes

            call nc_define_variable(ncid, var%ndims, var%name, var%long_name, var%units, &
                                    local_axes, file_desc%time_dimid, varid)

            deallocate (local_axes)
         else
            print *, "WARNING: Variable ", trim(var%name), " has no axes defined"
            return
         end if
      end if

      status = 0
   end function nc_define_variable_in_file

   !> End definition mode for a NetCDF file
   !>
   !> @param[in]  file_desc  File descriptor
   !> @return     Status code (0 = success)
   function nc_end_definition(file_desc) result(status)
      type(file_descriptor), intent(in) :: file_desc
      integer :: status

      if (file_desc%backend_id <= 0) then
         status = -1
         return
      end if

      status = nf90_enddef(file_desc%backend_id)
      if (status /= nf90_noerr) then
         print *, "WARNING: Error ending definition mode for: ", trim(file_desc%filename)
         print *, "         NetCDF error: ", trim(nf90_strerror(status))
         status = -1
      else
         status = 0
      end if
   end function nc_end_definition

   !> Write variable data to a file
   !>
   !> @param[in]  file_desc    File descriptor
   !> @param[in]  var          Variable to write
   !> @return     Status code (0 = success)
   function nc_write_variable_data(file_desc, var) result(status)
      type(file_descriptor), intent(in) :: file_desc
      type(io_variable), intent(in) :: var
      integer :: status

      integer :: ncid, varid, time_index

      status = -1
      ncid = file_desc%backend_id
      
      if (ncid <= 0) then
         print *, "ERROR: Invalid file for writing: ", trim(var%name)
         return
      end if

      time_index = file_desc%time_index

      ! Get variable ID in this file
      varid = get_varid_for_variable(var, ncid)
      if (varid <= 0) then
         print *, "WARNING: Variable ", trim(var%name), " not defined in file"
         return
      end if

      ! Write the variable data based on dimension
      select case (var%ndims)
      case (0)  ! Scalar
         if (associated(var%scalar)) then
            call nc_write_variable(ncid, varid, var%scalar, time_index)
            status = 0
         end if
      case (1)  ! 1D array
         if (associated(var%data_1d)) then
            call nc_write_variable(ncid, varid, var%data_1d, time_index)
            status = 0
         end if
      case (2)  ! 2D array
         if (associated(var%data_2d)) then
            call nc_write_variable(ncid, varid, var%data_2d, time_index)
            status = 0
         end if
      case (3)  ! 3D array
         if (associated(var%data_3d)) then
            call nc_write_variable(ncid, varid, var%data_3d, time_index)
            status = 0
         end if
      end select
   end function nc_write_variable_data

   !> Write time value to a file
   !>
   !> @param[inout] file_desc    File descriptor (time_index will be incremented)
   !> @param[in]    time_value   Current time value
   !> @return       Status code (0 = success)
   function nc_write_time(file_desc, time_value) result(status)
      type(file_descriptor), intent(inout) :: file_desc
      real, intent(in) :: time_value
      integer :: status

      integer :: ncid, ncstatus
      real :: time_slice(1)

      status = -1
      ncid = file_desc%backend_id

      if (ncid <= 0) return

      ! Check if we have a time variable
      if (file_desc%time_varid <= 0) then
         ! No time variable - not an error, just skip
         status = 0
         return
      end if

      ! Write time value
      time_slice(1) = time_value
      ncstatus = nf90_put_var(ncid, file_desc%time_varid, time_slice, &
                              start=[file_desc%time_index], count=[1])
      
      if (ncstatus /= nf90_noerr) then
         print *, "WARNING: Error writing time to: ", trim(file_desc%filename)
         print *, "         NetCDF error: ", trim(nf90_strerror(ncstatus))
         return
      end if

      ! Increment time index
      call file_desc%increment_time()

      status = 0
   end function nc_write_time

   !---------------------------------------------------------------------------
   ! Helper functions
   !---------------------------------------------------------------------------

   !> Get NetCDF variable ID for a variable in a file
   !>
   !> @param[in]  var   Variable to find
   !> @param[in]  ncid  NetCDF file ID
   !> @return     Variable ID or -1 if not found
   function get_varid_for_variable(var, ncid) result(varid)
      type(io_variable), intent(in) :: var
      integer, intent(in) :: ncid
      integer :: varid

      if (nf90_inq_varid(ncid, trim(var%name), varid) /= nf90_noerr) then
         varid = -1
      end if
   end function get_varid_for_variable

end module io_netcdf
