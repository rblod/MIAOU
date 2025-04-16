!===============================================================================
!> @file io_netcdf.F90
!>
!> NetCDF implementation of the I/O system
!>
!> This module provides the NetCDF-specific implementation of the I/O system.
!> It handles file creation, variable definition, and data writing using the
!> NetCDF library.
!>
!> @author Rachid Benshila
!> @date 2025-04-11
!===============================================================================
module io_netcdf
   use netcdf
   use netcdf_utils, only: nc_check
   use netcdf_backend, only: nc_define_variable, nc_write_variable
   use grid_module, only: grid, axis, create_axis
   use io_definitions, only: io_variable, file_descriptor, io_var_registry
   use io_config, only: get_output_prefix
   implicit none
   private

   ! Public procedures for NetCDF I/O operations
   public :: nc_initialize, nc_finalize
   public :: nc_create_file, nc_close_file
   public :: nc_write_variable_data, nc_write_time
   public :: generate_filename, nc_define_variable_in_file
   public :: nc_end_definition, get_varid_for_variable

   ! Constants
   real, parameter :: TOLERANCE = 1.0e-5

   ! Internal types for NetCDF implementation
   !> Extended file descriptor with NetCDF-specific information
   type, extends(file_descriptor) :: netcdf_file
      integer :: ncid = -1        !< NetCDF file ID
      integer :: time_dimid = -1  !< Time dimension ID in the file
      integer :: time_varid = -1  !< Time variable ID in the file
   end type netcdf_file

   ! Registry of open files
   type(netcdf_file), allocatable :: open_files(:)

contains

   !---------------------------------------------------------------------------
   ! Initialization and cleanup
   !---------------------------------------------------------------------------

   !> Initialize the NetCDF I/O system
   !>
   !> @return Status code (0 = success)
   function nc_initialize() result(status)
      integer :: status

      status = 0

      ! Initialize file registry
      if (allocated(open_files)) deallocate (open_files)

      ! Success
      status = 0
   end function nc_initialize

   !> Finalize the NetCDF I/O system
   !>
   !> This function closes all open files and releases resources.
   !>
   !> @return Status code (0 = success)
   function nc_finalize() result(status)
      integer :: status
      integer :: i

      status = 0

      ! Close all open files
      if (allocated(open_files)) then
         do i = 1, size(open_files)
            if (open_files(i)%ncid > 0) then
               call nc_check(nf90_close(open_files(i)%ncid))
               print *, "Closed file: ", trim(open_files(i)%filename)
            end if
         end do

         deallocate (open_files)
      end if
   end function nc_finalize

   !---------------------------------------------------------------------------
   ! File operations
   !---------------------------------------------------------------------------

   !> Create a NetCDF output file
   !>
   !> @param[in]  filename     Path to the file
   !> @param[in]  file_type    Type of file ("his", "avg", or "rst")
   !> @param[in]  freq         Output frequency in seconds
   !> @param[in]  time_units   Optional: time units
   !> @param[in]  calendar     Optional: calendar type
   !> @param[out] file_desc    File descriptor for the created file
   !> @return     Status code (0 = success)
   function nc_create_file(filename, file_type, freq, time_units, calendar, file_desc) result(status)
      character(len=*), intent(in) :: filename, file_type
      real, intent(in) :: freq
      character(len=*), intent(in), optional :: time_units, calendar
      type(file_descriptor), intent(out) :: file_desc
      integer :: status

      type(netcdf_file) :: nc_file
      integer :: ncid

      ! Initialize file descriptor
      nc_file%filename = filename
      nc_file%type = file_type
      nc_file%freq = freq
      nc_file%time_index = 1

      ! Create NetCDF file
      call nc_check(nf90_create(filename, nf90_clobber, ncid), &
                    "Create file: "//trim(filename))
      nc_file%ncid = ncid

      ! Create time dimension
      call nc_check(nf90_def_dim(ncid, "time", nf90_unlimited, nc_file%time_dimid), &
                    "Create time dimension")

      ! Create time variable if units provided
      if (present(time_units)) then
         call nc_check(nf90_def_var(ncid, "time", nf90_real, [nc_file%time_dimid], &
                                    nc_file%time_varid))
         call nc_check(nf90_put_att(ncid, nc_file%time_varid, "units", trim(time_units)))

         if (present(calendar)) then
            call nc_check(nf90_put_att(ncid, nc_file%time_varid, "calendar", trim(calendar)))
         end if

         ! Add standard_name attribute for CF compliance
         call nc_check(nf90_put_att(ncid, nc_file%time_varid, "standard_name", "time"))

         print *, "Created time variable in file: ", trim(filename), &
            " with units: ", trim(time_units)
      else
         print *, "Warning: No time units provided, time variable not created in file: ", &
            trim(filename)
      end if

      ! Add to open files list
      call add_to_open_files(nc_file)

      ! Copy values to output descriptor
      file_desc%filename = nc_file%filename
      file_desc%type = nc_file%type
      file_desc%freq = nc_file%freq
      file_desc%time_index = nc_file%time_index
      file_desc%backend_id = nc_file%ncid

      status = 0
   end function nc_create_file

   !> Close a NetCDF file
   !>
   !> @param[in] file_desc  File descriptor of the file to close
   !> @return    Status code (0 = success)
   function nc_close_file(file_desc) result(status)
      type(file_descriptor), intent(in) :: file_desc
      integer :: status
      integer :: i

      status = 0

      ! Find the file in the registry
      if (allocated(open_files)) then
         do i = 1, size(open_files)
            if (open_files(i)%ncid == file_desc%backend_id) then
               ! Close the file
               call nc_check(nf90_close(open_files(i)%ncid))
               print *, "Closed file: ", trim(open_files(i)%filename)

               ! Mark as closed
               open_files(i)%ncid = -1
               status = 0
               return
            end if
         end do
      end if

      ! File not found
      print *, "Warning: File not found in registry: ", trim(file_desc%filename)
      status = -1
   end function nc_close_file

   !> Add a file to the registry of open files
   !>
   !> @param[in] file  NetCDF file descriptor
   subroutine add_to_open_files(file)
      type(netcdf_file), intent(in) :: file
      type(netcdf_file), allocatable :: temp(:)
      integer :: n

      ! Add file to the list
      if (.not. allocated(open_files)) then
         allocate (open_files(1))
         open_files(1) = file
      else
         ! Resize the array to add one more element
         n = size(open_files)
         allocate (temp(n + 1))
         temp(1:n) = open_files
         call move_alloc(temp, open_files)

         ! Add new file
         open_files(n + 1) = file
      end if
   end subroutine add_to_open_files

   !> Generate a filename for an output file
   !>
   !> @param[in] var_prefix  Variable-specific prefix, or "" for global prefix
   !> @param[in] file_type   Type of file ("his", "avg", or "rst")
   !> @param[in] freq        Output frequency in seconds
   !> @return    Complete filename with appropriate prefix and frequency
   function generate_filename(var_prefix, file_type, freq) result(filename)
      character(len=*), intent(in) :: var_prefix, file_type
      real, intent(in) :: freq
      character(len=256) :: filename

      character(len=16) :: freq_str
      character(len=128) :: prefix

      ! Determine the base prefix
      if (trim(var_prefix) == "") then
         ! Global prefix from configuration
         prefix = get_output_prefix()
         print *, "  Using global prefix: ", trim(prefix)
      else
         ! Variable-specific prefix
         prefix = trim(var_prefix)
         print *, "  Using var-specific prefix: ", trim(prefix)
      end if

      ! Convert frequency to string and build filename
      if (freq > 0) then
         write (freq_str, '(I0)') nint(freq)
         filename = trim(prefix)//'_'//trim(file_type)//'_'//trim(freq_str)//'s.nc'
      else
         filename = trim(prefix)//'_'//trim(file_type)//'.nc'
      end if
   end function generate_filename

   !---------------------------------------------------------------------------
   ! Variable operations
   !---------------------------------------------------------------------------

   !> Define a variable in a NetCDF file
   !>
   !> This function creates dimensions and defines a variable in a NetCDF file
   !> based on the variable's metadata and dimensionality.
   !>
   !> @param[in] file_desc  File descriptor
   !> @param[in] var        Variable to define
   !> @return    Status code (0 = success)
   function nc_define_variable_in_file(file_desc, var) result(status)
      type(file_descriptor), intent(in) :: file_desc
      type(io_variable), intent(in) :: var
      integer :: status

      integer :: ncid, time_dimid, varid
      type(axis) :: dummy_axis(1)
      type(axis), allocatable :: local_axes(:)

      status = -1
      ncid = file_desc%backend_id

      ! Check file validity
      if (ncid <= 0) then
         print *, "Error: Invalid NetCDF file ID for: ", trim(file_desc%filename)
         return
      end if

      ! Get time dimension
      status = nf90_inquire(ncid, unlimitedDimId=time_dimid)
      if (status /= nf90_noerr) then
         print *, "Warning: Could not find unlimited dimension in file: ", trim(file_desc%filename)
         return
      end if

      ! Special case for scalar variables
      if (var%ndims == 0) then
         ! Create a dummy axis for scalars
         dummy_axis(1) = create_axis("dummy", "Dummy dimension", "count", 1)

         call nc_define_variable(ncid, 0, var%name, var%long_name, var%units, &
                                 dummy_axis, time_dimid, varid)
      else
         ! For variables with multiple dimensions
         if (allocated(var%var_grid%axes)) then
            allocate (local_axes(size(var%var_grid%axes)))
            local_axes = var%var_grid%axes

            call nc_define_variable(ncid, var%ndims, var%name, var%long_name, var%units, &
                                    local_axes, time_dimid, varid)

            deallocate (local_axes)
         else
            print *, "Warning: Variable ", trim(var%name), " has no axes defined"
            status = -1
            return
         end if
      end if

      status = 0
   end function nc_define_variable_in_file

   !> Write variable data to a file
   !>
   !> @param[in]     file_desc    File descriptor
   !> @param[in]     var          Variable to write
   !> @return        Status code (0 = success)
   function nc_write_variable_data(file_desc, var) result(status)
      type(file_descriptor), intent(in) :: file_desc
      type(io_variable), intent(in) :: var
      integer :: status

      integer :: ncid, varid, time_index

      status = -1

      ! Get file information
      ncid = get_ncid_from_descriptor(file_desc)
      if (ncid < 0) return

      time_index = get_time_index_from_ncid(ncid)

      ! Get variable ID in this file
      varid = get_varid_for_variable(var, ncid)
      if (varid <= 0) then
         print *, "Warning: Variable ", trim(var%name), " not defined in file"
         status = -1
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
   !> @param[in]     file_desc    File descriptor
   !> @param[in]     time_value   Current time value
   !> @return        Status code (0 = success)
   function nc_write_time(file_desc, time_value) result(status)
      type(file_descriptor), intent(in) :: file_desc
      real, intent(in) :: time_value
      integer :: status

      integer :: ncid, time_varid, time_index
      real :: time_slice(1)

      status = -1

      ! Get file information
      ncid = get_ncid_from_descriptor(file_desc)
      if (ncid < 0) return

      time_varid = get_time_varid_from_ncid(ncid)
      if (time_varid < 0) then
         print *, "Warning: No time variable found in file: ", trim(file_desc%filename)
         status = 0  ! Not an error, just no time variable
         return
      end if

      time_index = get_time_index_from_ncid(ncid)

      ! Write time value
      time_slice(1) = time_value
      print *, "Writing time value ", time_value, " to file: ", trim(file_desc%filename), &
         " at index ", time_index

      call nc_check(nf90_put_var(ncid, time_varid, time_slice, &
                                 start=[time_index], count=[1]), &
                    "Write time value")

      ! Increment time index
      call increment_time_index(ncid)

      status = 0
   end function nc_write_time

   !---------------------------------------------------------------------------
   ! Helper functions
   !---------------------------------------------------------------------------

   !> Get NetCDF ID from file descriptor
   !>
   !> @param[in]  file_desc  File descriptor
   !> @return     NetCDF ID or -1 if not found
   function get_ncid_from_descriptor(file_desc) result(ncid)
      type(file_descriptor), intent(in) :: file_desc
      integer :: ncid, i

      ncid = -1

      if (allocated(open_files)) then
         do i = 1, size(open_files)
            if (open_files(i)%ncid == file_desc%backend_id) then
               ncid = open_files(i)%ncid
               return
            end if
         end do
      end if
   end function get_ncid_from_descriptor

   !> Get time variable ID for a file
   !>
   !> @param[in]  ncid  NetCDF file ID
   !> @return     Time variable ID or -1 if not found
   function get_time_varid_from_ncid(ncid) result(time_varid)
      integer, intent(in) :: ncid
      integer :: time_varid, i

      time_varid = -1

      if (allocated(open_files)) then
         do i = 1, size(open_files)
            if (open_files(i)%ncid == ncid) then
               time_varid = open_files(i)%time_varid
               return
            end if
         end do
      end if
   end function get_time_varid_from_ncid

   !> Get current time index for a file
   !>
   !> @param[in]  ncid  NetCDF file ID
   !> @return     Time index or -1 if not found
   function get_time_index_from_ncid(ncid) result(time_index)
      integer, intent(in) :: ncid
      integer :: time_index, i

      time_index = -1

      if (allocated(open_files)) then
         do i = 1, size(open_files)
            if (open_files(i)%ncid == ncid) then
               time_index = open_files(i)%time_index
               return
            end if
         end do
      end if
   end function get_time_index_from_ncid

   !> Increment time index for a file
   !>
   !> @param[in]  ncid  NetCDF file ID
   subroutine increment_time_index(ncid)
      integer, intent(in) :: ncid
      integer :: i

      if (allocated(open_files)) then
         do i = 1, size(open_files)
            if (open_files(i)%ncid == ncid) then
               open_files(i)%time_index = open_files(i)%time_index + 1
               return
            end if
         end do
      end if
   end subroutine increment_time_index

   !> Get NetCDF variable ID for a variable in a file
   !>
   !> @param[in]  var   Variable to check
   !> @param[in]  ncid  NetCDF file ID
   !> @return     Variable ID or -1 if not found
   function get_varid_for_variable(var, ncid) result(varid)
      type(io_variable), intent(in) :: var
      integer, intent(in) :: ncid
      integer :: varid

      varid = -1

      ! Try to find the variable in the file
      if (nf90_inq_varid(ncid, trim(var%name), varid) == nf90_noerr) then
         return
      end if

      ! If not found, return -1
      varid = -1
   end function get_varid_for_variable

   !> End definition mode for a NetCDF file
   !>
   !> @param[in]  file_desc  File descriptor
   !> @return     Status code (0 = success)
   function nc_end_definition(file_desc) result(status)
      type(file_descriptor), intent(in) :: file_desc
      integer :: status

      status = nf90_enddef(file_desc%backend_id)
   end function nc_end_definition

end module io_netcdf
