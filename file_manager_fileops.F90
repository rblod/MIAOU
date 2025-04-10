!===============================================================================
!> @file file_manager_fileops.F90
!>
!> Submodule for file operations
!>
!> This submodule handles file-related operations such as filename generation,
!> file opening, and variable definition in NetCDF files.
!>
!> @author Rachid Benshila
!> @date 2025-04-10
!===============================================================================
submodule (file_manager) file_manager_fileops
contains

   !> Generate a filename for an output file
   !>
   !> @param[in] var_prefix  Variable-specific prefix, or "" for global prefix
   !> @param[in] file_type   Type of file ("his", "avg", or "rst")
   !> @param[in] freq        Output frequency in seconds
   !> @return    Complete filename with appropriate prefix and frequency
   module procedure generate_filename
      character(len=16) :: freq_str
      character(len=128) :: prefix

      ! Determine the base prefix
      if (trim(var_prefix) == "") then
         ! Global prefix from namelist
         select case (trim(file_type))
         case ("his")
            prefix = his_prefix
         case ("avg")
            prefix = avg_prefix
         case ("rst")
            prefix = rst_prefix
         case default
            prefix = "output"
         end select
      else
         ! Variable-specific prefix
         prefix = trim(var_prefix)
      end if

      ! Convert frequency to string and build filename
      if (freq > 0) then
         write (freq_str, '(I0)') nint(freq)
         filename = trim(prefix)//'_'//trim(file_type)//'_'//trim(freq_str)//'s.nc'
      else
         filename = trim(prefix)//'_'//trim(file_type)//'.nc'
      end if
   end procedure generate_filename

   !> Add a file to the open_files array
   !>
   !> @param[in] filename   Complete path to the NetCDF file
   !> @param[in] file_type  Type of file ("his", "avg", or "rst")
   !> @param[in] freq       Output frequency in seconds
   !> @param[in] ncid       NetCDF file ID
   module procedure add_to_open_files
      type(output_file), allocatable :: temp(:)
      integer :: n

      ! Add file to the list
      if (.not. allocated(open_files)) then
         allocate (open_files(1))
         open_files(1)%filename = filename
         open_files(1)%type = file_type
         open_files(1)%freq = freq
         open_files(1)%ncid = ncid
         open_files(1)%time_index = 1
      else
         ! Resize the array to add one more element
         n = size(open_files)
         allocate (temp(n + 1))
         temp(1:n) = open_files
         call move_alloc(temp, open_files)

         ! Initialize the new element
         open_files(n + 1)%filename = filename
         open_files(n + 1)%type = file_type
         open_files(n + 1)%freq = freq
         open_files(n + 1)%ncid = ncid
         open_files(n + 1)%time_index = 1
      end if
   end procedure add_to_open_files

   !> Define variables in a NetCDF file
   !>
   !> @param[in] ncid  NetCDF file ID
   !> @param[in] tag   File type ("his", "avg", or "rst")
   module procedure define_output_file
      integer :: i, file_idx
      integer :: time_dimid_local

      ! Find the appropriate file
      do i = 1, size(open_files)
         if (open_files(i)%ncid == ncid) then
            file_idx = i
            time_dimid_local = open_files(i)%time_dimid
            exit
         end if
      end do

      ! Define all applicable variables
      do i = 1, size(registered_vars)
         ! Check if this variable should be written to this type of file
         select case (tag)
         case ("his")
            if (.not. registered_vars(i)%to_his) cycle
         case ("avg")
            if (.not. registered_vars(i)%to_avg) cycle
         case ("rst")
            if (.not. registered_vars(i)%to_rst) cycle
         end select

         ! Define the variable using the NetCDF backend
         call define_single_variable(ncid, tag, i)
         
         ! For average files, allocate averaging buffers
         if (tag == "avg") then
            call registered_vars(i)%allocate_buffers()
         end if
      end do
   end procedure define_output_file

   !> Define a single variable in a NetCDF file
   !>
   !> @param[in] ncid     NetCDF file ID
   !> @param[in] tag      File type ("his", "avg", or "rst")
   !> @param[in] var_idx  Index of the variable to define
   module procedure define_single_variable
      integer :: file_idx, time_dimid
      type(axis) :: dummy_axis(1)

      ! Find the file's time dimension ID
      do file_idx = 1, size(open_files)
         if (open_files(file_idx)%ncid == ncid) then
            time_dimid = open_files(file_idx)%time_dimid
            exit
         end if
      end do

      ! Special handling for scalar variables
      if (registered_vars(var_idx)%ndims == 0) then
         ! Create a dummy axis if no axes are defined for scalar
         dummy_axis(1) = create_axis("dummy", "Dummy dimension", "count", 1)
      end if

      ! Define the variable using the backend
      select case (tag)
      case ("his")
         if (registered_vars(var_idx)%ndims == 0) then
            call nc_define_variable(ncid, registered_vars(var_idx)%ndims, &
                                 registered_vars(var_idx)%name, &
                                 registered_vars(var_idx)%long_name, &
                                 registered_vars(var_idx)%units, &
                                 dummy_axis, &
                                 time_dimid, &
                                 registered_vars(var_idx)%varid_his)
            print *, "Defined HIS scalar variable ", trim(registered_vars(var_idx)%name), " in file with ID ", ncid
         else
            call nc_define_variable(ncid, registered_vars(var_idx)%ndims, &
                                 registered_vars(var_idx)%name, &
                                 registered_vars(var_idx)%long_name, &
                                 registered_vars(var_idx)%units, &
                                 registered_vars(var_idx)%var_grid%axes, &
                                 time_dimid, &
                                 registered_vars(var_idx)%varid_his)
         end if

      case ("avg")
         if (registered_vars(var_idx)%ndims == 0) then
            call nc_define_variable(ncid, registered_vars(var_idx)%ndims, &
                                 registered_vars(var_idx)%name, &
                                 registered_vars(var_idx)%long_name, &
                                 registered_vars(var_idx)%units, &
                                 dummy_axis, &
                                 time_dimid, &
                                 registered_vars(var_idx)%varid_avg)
            print *, "Defined AVG scalar variable ", trim(registered_vars(var_idx)%name), " in file with ID ", ncid
         else
            call nc_define_variable(ncid, registered_vars(var_idx)%ndims, &
                                 registered_vars(var_idx)%name, &
                                 registered_vars(var_idx)%long_name, &
                                 registered_vars(var_idx)%units, &
                                 registered_vars(var_idx)%var_grid%axes, &
                                 time_dimid, &
                                 registered_vars(var_idx)%varid_avg)
         end if

      case ("rst")
         if (registered_vars(var_idx)%ndims == 0) then
            call nc_define_variable(ncid, registered_vars(var_idx)%ndims, &
                                 registered_vars(var_idx)%name, &
                                 registered_vars(var_idx)%long_name, &
                                 registered_vars(var_idx)%units, &
                                 dummy_axis, &
                                 time_dimid, &
                                 registered_vars(var_idx)%varid_rst)
            print *, "Defined RST scalar variable ", trim(registered_vars(var_idx)%name), " in file with ID ", ncid
         else
            call nc_define_variable(ncid, registered_vars(var_idx)%ndims, &
                                 registered_vars(var_idx)%name, &
                                 registered_vars(var_idx)%long_name, &
                                 registered_vars(var_idx)%units, &
                                 registered_vars(var_idx)%var_grid%axes, &
                                 time_dimid, &
                                 registered_vars(var_idx)%varid_rst)
         end if
      end select
   end procedure define_single_variable

end submodule file_manager_fileops