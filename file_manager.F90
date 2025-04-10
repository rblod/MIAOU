!===============================================================================
!> @file file_manager.F90
!>
!> Enhanced output manager supporting multiple dimensions (0D, 1D, 2D, 3D)
!>
!> This module provides a comprehensive system for managing NetCDF output files
!> for variables of different dimensions. It handles variable registration,
!> file creation, data writing, and memory management for averaging buffers.
!> The module supports history, average, and restart output types.
!>
!> @author Rachid Benshila
!> @date 2025-04-10
!===============================================================================
module file_manager
   use netcdf
   use grid_module
   use namelist_output, only: his_prefix, avg_prefix, rst_prefix
   use netcdf_backend, only: nc_check, nc_define_variable, nc_write_variable

   implicit none
   private

   public :: register_variable, define_output_file, write_output, generate_filename
   public :: initialize_output_files, finalize_output, write_all_outputs
   public :: nc_var

   !> @type nc_var
   !> Enhanced variable type supporting multiple dimensions
   !>
   !> This type represents a model variable that can be output to NetCDF files.
   !> It supports scalar (0D), vector (1D), matrix (2D), and volume (3D) data
   !> with configurable output settings for history, average, and restart files.
   type :: nc_var
      character(len=32) :: name            !< Variable name
      character(len=64) :: long_name       !< Long descriptive name
      character(len=32) :: units           !< Variable units
      type(grid) :: var_grid               !< Grid associated with the variable

      !< Dimension count (0=scalar, 1=1D, 2=2D, 3=3D)
      integer :: ndims = 0

      !< NetCDF variable IDs for different file types
      integer :: varid_his = -1            !< ID in history file
      integer :: varid_avg = -1            !< ID in average file
      integer :: varid_rst = -1            !< ID in restart file

      !< Output flags
      logical :: to_his = .false.          !< Write to history file
      logical :: to_avg = .false.          !< Write to average file
      logical :: to_rst = .false.          !< Write to restart file

      !< Data pointers for different dimensions - only one will be associated
      real, pointer :: scalar => null()             !< 0D data
      real, pointer :: data_1d(:) => null()         !< 1D data
      real, pointer :: data_2d(:, :) => null()       !< 2D data
      real, pointer :: data_3d(:, :, :) => null()     !< 3D data

      !< Averaging buffers with matching dimension
      real :: scalar_avg = 0.0                      !< For 0D
      real, allocatable :: avg_buffer_1d(:)         !< For 1D
      real, allocatable :: avg_buffer_2d(:, :)       !< For 2D
      real, allocatable :: avg_buffer_3d(:, :, :)     !< For 3D

      !< Time and frequency settings
      integer :: current_time_index = 1              !< Current time index
      real :: freq_his = -1.                         !< Frequency for history (seconds)
      real :: freq_avg = -1.                         !< Frequency for average (seconds)
      real :: freq_rst = -1.                         !< Frequency for restart (seconds)
      character(len=128) :: file_prefix = ""         !< Custom file prefix

      !< Memory management flag
      logical :: owns_avg_buffers = .false.          !< Indicates if this instance owns its buffers

   contains
      !< Allocate average buffers if needed
      procedure :: allocate_buffers

      !< Free average buffers if the variable owns them
      procedure :: deallocate_buffers

      !< Reset average buffers to zero without deallocating
      procedure :: reset_avg_buffers

   end type nc_var

   !> @type output_file
   !> Structure for tracking open NetCDF files
   !>
   !> This type maintains information about each open output file,
   !> including its type, frequency, and current time index.
   type :: output_file
      character(len=256) :: filename  !< Complete filename
      character(len=16) :: type       !< "his", "avg", or "rst"
      real :: freq                    !< Write frequency (seconds)
      integer :: ncid                 !< NetCDF file ID
      integer :: time_dimid           !< Time dimension ID
      integer :: time_varid           !< Time variable ID
      integer :: time_index = 1       !< Current time write index
   end type output_file

   !< Array of all registered variables
   type(nc_var), allocatable :: registered_vars(:)

   !< Array of all open output files
   type(output_file), allocatable :: open_files(:)

contains

   !---------------------------------------------------------------------------
   ! Filename generation
   !---------------------------------------------------------------------------

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
   end function generate_filename

   !---------------------------------------------------------------------------
   ! File management
   !---------------------------------------------------------------------------

   !> Add a file to the open_files array
   !>
   !> @param[in] filename   Complete path to the NetCDF file
   !> @param[in] file_type  Type of file ("his", "avg", or "rst")
   !> @param[in] freq       Output frequency in seconds
   !> @param[in] ncid       NetCDF file ID
   subroutine add_to_open_files(filename, file_type, freq, ncid)
      character(len=*), intent(in) :: filename, file_type
      real, intent(in) :: freq
      integer, intent(in) :: ncid
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
         allocate (temp(n))
         temp = open_files
         deallocate (open_files)
         allocate (open_files(n + 1))
         open_files(1:n) = temp

         ! Initialize the new element
         open_files(n + 1)%filename = filename
         open_files(n + 1)%type = file_type
         open_files(n + 1)%freq = freq
         open_files(n + 1)%ncid = ncid
         open_files(n + 1)%time_index = 1
      end if
   end subroutine add_to_open_files

   !---------------------------------------------------------------------------
   ! Variable registration
   !---------------------------------------------------------------------------

   !> Register a variable for output with pre-allocation strategy
   !>
   !> This enhanced version pre-allocates space for multiple variables and
   !> reuses empty slots before growing the array.
   !>
   !> @param[in] v  Variable to register
   subroutine register_variable(v)
      type(nc_var), intent(in) :: v
      type(nc_var), allocatable :: tmp(:)
      integer :: i, n, new_size, var_index

      if (.not. allocated(registered_vars)) then
         ! Initial allocation with extra space
         allocate (registered_vars(10))  ! Pre-allocate for 10 variables
         registered_vars(1) = v

         ! Associate the appropriate pointer
         call associate_variable_data(registered_vars(1), v)

         ! Explicitly set the index of the added variable
         var_index = 1

         ! Mark other elements as unused
         do i = 2, 10
            registered_vars(i)%name = ""  ! Mark as unused
         end do
      else
         ! Find an empty slot or extend the array
         n = size(registered_vars)
         var_index = -1  ! Initialize to an invalid value

         ! Look for an empty slot
         do i = 1, n
            if (trim(registered_vars(i)%name) == "") then
               registered_vars(i) = v
               call associate_variable_data(registered_vars(i), v)

               ! Record the index of the added variable
               var_index = i

               print *
               print *, 'FOUND EMPTY SLOT AT INDEX ', var_index, ' FOR ', v%name
               print *
               exit  ! Exit the loop but continue execution
            end if
         end do

         ! If no empty slot was found
         if (var_index == -1) then
            ! No empty slot, grow the array
            new_size = n + max(n/2, 10)  ! Geometric growth strategy
            allocate (tmp(new_size))
            tmp(1:n) = registered_vars
            tmp(n + 1) = v

            ! Mark new elements as unused
            do i = n + 2, new_size
               tmp(i)%name = ""
            end do

            call move_alloc(tmp, registered_vars)
            call associate_variable_data(registered_vars(n + 1), v)

            ! Set the index of the added variable
            var_index = n + 1
         end if
      end if

      ! Verify that the index is valid before calling allocate_buffers
      if (var_index > 0 .and. var_index <= size(registered_vars)) then
         print *, 'ALLOCATING BUFFERS FOR VAR AT INDEX ', var_index, ': ', registered_vars(var_index)%name
         call registered_vars(var_index)%allocate_buffers()
      else
         print *, 'ERROR: Invalid variable index ', var_index, ' for ', v%name
      end if

      print *
      print *, 'SUBROUTINE REGISTRED_VARIABLE_COMPLETED FOR ', v%name
      print *

   end subroutine register_variable

   !> Helper procedure to associate variable data pointers
   !>
   !> @param[inout] target_var  Target variable to receive data pointers
   !> @param[in]    source_var  Source variable with data pointers to copy
   subroutine associate_variable_data(target_var, source_var)
      type(nc_var), intent(inout) :: target_var
      type(nc_var), intent(in) :: source_var

      select case (source_var%ndims)
      case (0)
         if (associated(source_var%scalar)) then
            target_var%scalar => source_var%scalar
         end if
      case (1)
         if (associated(source_var%data_1d)) then
            target_var%data_1d => source_var%data_1d
         end if
      case (2)
         if (associated(source_var%data_2d)) then
            target_var%data_2d => source_var%data_2d
         end if
      case (3)
         if (associated(source_var%data_3d)) then
            target_var%data_3d => source_var%data_3d
         end if
      end select
   end subroutine associate_variable_data

   !> Allocate averaging buffers if needed
   !>
   !> @param[inout] this  Variable to allocate buffers for
   subroutine allocate_buffers(this)
      class(nc_var), intent(inout) :: this

      ! Only allocate if average mode is enabled
      if (.not. this%to_avg) return

      ! Based on variable dimension
      select case (this%ndims)
      case (0)
         ! For scalars, just reset the value
         this%scalar_avg = 0.0
      case (1)
         ! For 1D data
         if (associated(this%data_1d)) then
            if (.not. allocated(this%avg_buffer_1d)) then
               allocate (this%avg_buffer_1d(size(this%data_1d)))
               this%avg_buffer_1d = 0.0
               this%owns_avg_buffers = .true.
            end if
         end if
      case (2)
         ! For 2D data
         if (associated(this%data_2d)) then
            if (.not. allocated(this%avg_buffer_2d)) then
               allocate (this%avg_buffer_2d(size(this%data_2d, 1), size(this%data_2d, 2)))
               this%avg_buffer_2d = 0.0
               this%owns_avg_buffers = .true.
            end if
         end if
      case (3)
         ! For 3D data
         if (associated(this%data_3d)) then
            if (.not. allocated(this%avg_buffer_3d)) then
               allocate (this%avg_buffer_3d(size(this%data_3d, 1), &
                                            size(this%data_3d, 2), &
                                            size(this%data_3d, 3)))
               this%avg_buffer_3d = 0.0
               this%owns_avg_buffers = .true.
            end if
         end if
      end select
   end subroutine allocate_buffers

   !> Deallocate averaging buffers if the variable owns them
   !>
   !> @param[inout] this  Variable to deallocate buffers for
   subroutine deallocate_buffers(this)
      class(nc_var), intent(inout) :: this

      ! Only deallocate if we own the buffers
      if (.not. this%owns_avg_buffers) return

      ! Based on dimension
      select case (this%ndims)
      case (1)
         if (allocated(this%avg_buffer_1d)) then
            deallocate (this%avg_buffer_1d)
         end if
      case (2)
         if (allocated(this%avg_buffer_2d)) then
            deallocate (this%avg_buffer_2d)
         end if
      case (3)
         if (allocated(this%avg_buffer_3d)) then
            deallocate (this%avg_buffer_3d)
         end if
      end select

      this%owns_avg_buffers = .false.
   end subroutine deallocate_buffers

   !> Reset averaging buffers to zero without deallocating
   !>
   !> @param[inout] this  Variable to reset buffers for
   subroutine reset_avg_buffers(this)
      class(nc_var), intent(inout) :: this

      ! Reset based on dimension
      select case (this%ndims)
      case (0)
         this%scalar_avg = 0.0
      case (1)
         if (allocated(this%avg_buffer_1d)) then
            this%avg_buffer_1d = 0.0
         end if
      case (2)
         if (allocated(this%avg_buffer_2d)) then
            this%avg_buffer_2d = 0.0
         end if
      case (3)
         if (allocated(this%avg_buffer_3d)) then
            this%avg_buffer_3d = 0.0
         end if
      end select
   end subroutine reset_avg_buffers

   !---------------------------------------------------------------------------
   ! Variable definition
   !---------------------------------------------------------------------------

   !> Define variables in a NetCDF file
   !>
   !> @param[in] ncid  NetCDF file ID
   !> @param[in] tag   File type ("his", "avg", or "rst")
   subroutine define_output_file(ncid, tag)
      integer, intent(in) :: ncid
      character(len=*), intent(in) :: tag
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
         select case (tag)
         case ("his")
            call nc_define_variable(ncid, registered_vars(i)%ndims, &
                                    registered_vars(i)%name, &
                                    registered_vars(i)%long_name, &
                                    registered_vars(i)%units, &
                                    registered_vars(i)%var_grid%axes, &
                                    time_dimid_local, &
                                    registered_vars(i)%varid_his)

            ! Allocate averaging buffers if needed
            if (tag == "avg") then
               select case (registered_vars(i)%ndims)
               case (0)
                  registered_vars(i)%scalar_avg = 0.0
               case (1)
                  if (.not. allocated(registered_vars(i)%avg_buffer_1d)) then
                     allocate (registered_vars(i)%avg_buffer_1d(size(registered_vars(i)%data_1d)))
                     registered_vars(i)%avg_buffer_1d = 0.0
                  end if
               case (2)
                  if (.not. allocated(registered_vars(i)%avg_buffer_2d)) then
                     allocate (registered_vars(i)%avg_buffer_2d( &
                               size(registered_vars(i)%data_2d, 1), &
                               size(registered_vars(i)%data_2d, 2)))
                     registered_vars(i)%avg_buffer_2d = 0.0
                  end if
               case (3)
                  if (.not. allocated(registered_vars(i)%avg_buffer_3d)) then
                     allocate (registered_vars(i)%avg_buffer_3d( &
                               size(registered_vars(i)%data_3d, 1), &
                               size(registered_vars(i)%data_3d, 2), &
                               size(registered_vars(i)%data_3d, 3)))
                     registered_vars(i)%avg_buffer_3d = 0.0
                  end if
               end select
            end if

         case ("avg")
            call nc_define_variable(ncid, registered_vars(i)%ndims, &
                                    registered_vars(i)%name, &
                                    registered_vars(i)%long_name, &
                                    registered_vars(i)%units, &
                                    registered_vars(i)%var_grid%axes, &
                                    time_dimid_local, &
                                    registered_vars(i)%varid_avg)

            ! Allocate averaging buffers based on dimension
            select case (registered_vars(i)%ndims)
            case (0)
               registered_vars(i)%scalar_avg = 0.0
            case (1)
               if (.not. allocated(registered_vars(i)%avg_buffer_1d)) then
                  allocate (registered_vars(i)%avg_buffer_1d(size(registered_vars(i)%data_1d)))
                  registered_vars(i)%avg_buffer_1d = 0.0
               end if
            case (2)
               if (.not. allocated(registered_vars(i)%avg_buffer_2d)) then
                  allocate (registered_vars(i)%avg_buffer_2d( &
                            size(registered_vars(i)%data_2d, 1), &
                            size(registered_vars(i)%data_2d, 2)))
                  registered_vars(i)%avg_buffer_2d = 0.0
               end if
            case (3)
               if (.not. allocated(registered_vars(i)%avg_buffer_3d)) then
                  allocate (registered_vars(i)%avg_buffer_3d( &
                            size(registered_vars(i)%data_3d, 1), &
                            size(registered_vars(i)%data_3d, 2), &
                            size(registered_vars(i)%data_3d, 3)))
                  registered_vars(i)%avg_buffer_3d = 0.0
               end if
            end select

         case ("rst")
            call nc_define_variable(ncid, registered_vars(i)%ndims, &
                                    registered_vars(i)%name, &
                                    registered_vars(i)%long_name, &
                                    registered_vars(i)%units, &
                                    registered_vars(i)%var_grid%axes, &
                                    time_dimid_local, &
                                    registered_vars(i)%varid_rst)
         end select
      end do
   end subroutine define_output_file

   !---------------------------------------------------------------------------
   ! Variable writing
   !---------------------------------------------------------------------------

   !> Write data to output files
   !>
   !> @param[in] ncid        NetCDF file ID
   !> @param[in] tag         File type ("his", "avg", or "rst")
   !> @param[in] time_value  Current model time in seconds
   subroutine write_output(ncid, tag, time_value)
      use ocean_var, only: dt  ! Import time step from ocean_var
      integer, intent(in) :: ncid
      character(len=*), intent(in) :: tag
      real, intent(in) :: time_value
      integer :: i, file_idx
      real :: time_slice(1)
      logical :: should_write, any_written
      integer :: steps_since_last_write
      real :: freq
      real, parameter :: TOL = 1.0e-5
      character(len=256) :: current_filename
      integer :: varid

      ! Find the file index
      file_idx = -1
      do i = 1, size(open_files)
         if (open_files(i)%ncid == ncid) then
            file_idx = i
            current_filename = open_files(i)%filename
            exit
         end if
      end do

      if (file_idx == -1) then
         print *, "Error: File not found in open_files list"
         return
      end if

      any_written = .false.

      ! Process all registered variables
      do i = 1, size(registered_vars)
         should_write = .false.

         ! Check if this variable should be written to this type of file
         select case (tag)
         case ("his")
            if (.not. registered_vars(i)%to_his) cycle
            if (registered_vars(i)%varid_his <= 0) cycle
            freq = registered_vars(i)%freq_his
            varid = registered_vars(i)%varid_his
         case ("avg")
            if (.not. registered_vars(i)%to_avg) cycle
            if (registered_vars(i)%varid_avg <= 0) cycle
            freq = registered_vars(i)%freq_avg
            varid = registered_vars(i)%varid_avg
         case ("rst")
            if (registered_vars(i)%varid_rst <= 0) cycle
            freq = registered_vars(i)%freq_rst
            varid = registered_vars(i)%varid_rst
         end select

         ! Check file-variable correspondence
         ! First check if this is a variable with its own prefix
         if (trim(registered_vars(i)%file_prefix) /= "") then
            ! Variable has custom prefix - check if this file matches it
            if (index(current_filename, trim(registered_vars(i)%file_prefix)) /= 1) then
               cycle  ! Not the file for this variable
            end if
         else
            ! Variable uses global prefix - check if file type matches
            select case (tag)
            case ("his")
               if (index(current_filename, trim(his_prefix)) /= 1) cycle
            case ("avg")
               if (index(current_filename, trim(avg_prefix)) /= 1) cycle
            case ("rst")
               if (index(current_filename, trim(rst_prefix)) /= 1) cycle
            end select
         end if

         ! Handle averaging for avg files
         if (tag == "avg" .and. registered_vars(i)%to_avg) then
            ! Accumulate data based on dimension
            select case (registered_vars(i)%ndims)
            case (0)
               if (associated(registered_vars(i)%scalar)) then
                  registered_vars(i)%scalar_avg = registered_vars(i)%scalar_avg + &
                                                  registered_vars(i)%scalar
               end if
            case (1)
               if (associated(registered_vars(i)%data_1d)) then
                  registered_vars(i)%avg_buffer_1d = registered_vars(i)%avg_buffer_1d + &
                                                     registered_vars(i)%data_1d
               end if
            case (2)
               if (associated(registered_vars(i)%data_2d)) then
                  print *, size(registered_vars(i)%avg_buffer_2d)
                  print *, size(registered_vars(i)%data_2d)
                  registered_vars(i)%avg_buffer_2d = registered_vars(i)%avg_buffer_2d + &
                                                     registered_vars(i)%data_2d
               end if
            case (3)
               if (associated(registered_vars(i)%data_3d)) then
                  registered_vars(i)%avg_buffer_3d = registered_vars(i)%avg_buffer_3d + &
                                                     registered_vars(i)%data_3d
               end if
            end select
         end if

         ! Determine if we should write at this time step
         if (abs(time_value) < TOL) then
            ! First time step
            if (tag == "his") then
               ! For "his" files - always write
               should_write = .true.
            else
               ! For "avg" and "rst" files - don't write at time 0
               should_write = .false.
            end if
         else
            ! Other time steps - check frequency
            should_write = abs(mod(time_value, freq)) < TOL
         end if

         if (should_write) any_written = .true.
      end do

      ! If nothing to write, exit
      if (.not. any_written) return

      ! Write time value
      if (open_files(file_idx)%time_varid /= -1) then
         time_slice(1) = time_value
         call nc_check(nf90_put_var(ncid, open_files(file_idx)%time_varid, time_slice, &
                                    start=[open_files(file_idx)%time_index], count=[1]))
      end if

      ! Process all registered variables again to write them
      do i = 1, size(registered_vars)
         should_write = .false.

         ! Check if this variable should be written to this type of file
         select case (tag)
         case ("his")
            if (.not. registered_vars(i)%to_his) cycle
            if (registered_vars(i)%varid_his <= 0) cycle
            freq = registered_vars(i)%freq_his
            varid = registered_vars(i)%varid_his
         case ("avg")
            if (.not. registered_vars(i)%to_avg) cycle
            if (registered_vars(i)%varid_avg <= 0) cycle
            freq = registered_vars(i)%freq_avg
            varid = registered_vars(i)%varid_avg
         case ("rst")
            if (.not. registered_vars(i)%to_rst) cycle
            if (registered_vars(i)%varid_rst <= 0) cycle
            freq = registered_vars(i)%freq_rst
            varid = registered_vars(i)%varid_rst
         end select

         ! Check file-variable correspondence
         ! First check if this is a variable with its own prefix
         if (trim(registered_vars(i)%file_prefix) /= "") then
            ! Variable has custom prefix - check if this file matches it
            if (index(current_filename, trim(registered_vars(i)%file_prefix)) /= 1) then
               cycle  ! Not the file for this variable
            end if
         else
            ! Variable uses global prefix - check if file type matches
            select case (tag)
            case ("his")
               if (index(current_filename, trim(his_prefix)) /= 1) cycle
            case ("avg")
               if (index(current_filename, trim(avg_prefix)) /= 1) cycle
            case ("rst")
               if (index(current_filename, trim(rst_prefix)) /= 1) cycle
            end select
         end if

         ! Determine if we should write at this time step
         if (abs(time_value) < TOL) then
            ! First time step
            if (tag == "his") then
               ! For "his" files - always write
               should_write = .true.
            else
               ! For "avg" and "rst" files - don't write at time 0
               should_write = .false.
            end if
         else
            ! Other time steps - check frequency
            should_write = abs(mod(time_value, freq)) < TOL
         end if

         ! Write if needed
         if (should_write) then
            print *, "Writing ", trim(registered_vars(i)%name), " to ", trim(tag), &
               " file at time=", time_value, " index=", open_files(file_idx)%time_index, &
               " file=", trim(current_filename)

            select case (tag)
            case ("his")
               ! Write history data based on dimension
               select case (registered_vars(i)%ndims)
               case (0)
                  if (associated(registered_vars(i)%scalar)) then
                     call nc_write_variable(ncid, varid, registered_vars(i)%scalar, &
                                            open_files(file_idx)%time_index)
                  end if
               case (1)
                  if (associated(registered_vars(i)%data_1d)) then
                     call nc_write_variable(ncid, varid, registered_vars(i)%data_1d, &
                                            open_files(file_idx)%time_index)
                  end if
               case (2)
                  if (associated(registered_vars(i)%data_2d)) then
                     call nc_write_variable(ncid, varid, registered_vars(i)%data_2d, &
                                            open_files(file_idx)%time_index)
                  end if
               case (3)
                  if (associated(registered_vars(i)%data_3d)) then
                     call nc_write_variable(ncid, varid, registered_vars(i)%data_3d, &
                                            open_files(file_idx)%time_index)
                  end if
               end select

            case ("avg")
               ! Determine steps since last write for averaging
               if (abs(time_value) < TOL) then
                  steps_since_last_write = 1
               else
                  steps_since_last_write = nint(freq/dt)
               end if

               ! Write average data based on dimension
               select case (registered_vars(i)%ndims)
               case (0)
                  if (abs(registered_vars(i)%scalar_avg) > TOL) then
                     call nc_write_variable(ncid, varid, &
                                            registered_vars(i)%scalar_avg/real(steps_since_last_write), &
                                            open_files(file_idx)%time_index)
                     call registered_vars(i)%reset_avg_buffers()
                  end if
               case (1)
                  if (allocated(registered_vars(i)%avg_buffer_1d)) then
                     if (maxval(abs(registered_vars(i)%avg_buffer_1d)) > TOL) then
                        call nc_write_variable(ncid, varid, &
                                               registered_vars(i)%avg_buffer_1d/real(steps_since_last_write), &
                                               open_files(file_idx)%time_index)
                        call registered_vars(i)%reset_avg_buffers()
                     end if
                  end if
               case (2)
                  if (allocated(registered_vars(i)%avg_buffer_2d)) then
                     if (maxval(abs(registered_vars(i)%avg_buffer_2d)) > TOL) then
                        call nc_write_variable(ncid, varid, &
                                               registered_vars(i)%avg_buffer_2d/real(steps_since_last_write), &
                                               open_files(file_idx)%time_index)
                        call registered_vars(i)%reset_avg_buffers()
                     end if
                  end if
               case (3)
                  if (allocated(registered_vars(i)%avg_buffer_3d)) then
                     if (maxval(abs(registered_vars(i)%avg_buffer_3d)) > TOL) then
                        call nc_write_variable(ncid, varid, &
                                               registered_vars(i)%avg_buffer_3d/real(steps_since_last_write), &
                                               open_files(file_idx)%time_index)
                        call registered_vars(i)%reset_avg_buffers()
                     end if
                  end if
               end select

            case ("rst")
               ! Write restart data based on dimension
               select case (registered_vars(i)%ndims)
               case (0)
                  if (associated(registered_vars(i)%scalar)) then
                     call nc_write_variable(ncid, varid, registered_vars(i)%scalar, &
                                            open_files(file_idx)%time_index)
                  end if
               case (1)
                  if (associated(registered_vars(i)%data_1d)) then
                     call nc_write_variable(ncid, varid, registered_vars(i)%data_1d, &
                                            open_files(file_idx)%time_index)
                  end if
               case (2)
                  if (associated(registered_vars(i)%data_2d)) then
                     call nc_write_variable(ncid, varid, registered_vars(i)%data_2d, &
                                            open_files(file_idx)%time_index)
                  end if
               case (3)
                  if (associated(registered_vars(i)%data_3d)) then
                     call nc_write_variable(ncid, varid, registered_vars(i)%data_3d, &
                                            open_files(file_idx)%time_index)
                  end if
               end select
            end select
         end if
      end do

      ! Increment time index for all file types if anything was written
      if (any_written) then
         open_files(file_idx)%time_index = open_files(file_idx)%time_index + 1
         print *, "Incremented time index to ", open_files(file_idx)%time_index, &
            " for file ", trim(current_filename)
      end if
   end subroutine write_output

   !---------------------------------------------------------------------------
   ! File initialization and cleanup
   !---------------------------------------------------------------------------

   !> Initialize all output files
   !>
   !> @param[in] time_units  Optional: Time units string (e.g., "seconds since...")
   !> @param[in] calendar    Optional: Calendar type
   subroutine initialize_output_files(time_units, calendar)
      character(len=*), intent(in), optional :: time_units, calendar
      integer :: i, j, ncid
      real :: freq
      character(len=16) :: file_types(3) = ["his", "avg", "rst"]
      character(len=256) :: filename
      logical :: file_exists
      integer :: file_idx

      ! Check if variables are registered
      if (.not. allocated(registered_vars)) then
         print *, "Warning: No variables registered before initializing output files"
         return
      end if

      ! Process each variable
      do i = 1, size(registered_vars)
         ! For each output file type
         do j = 1, size(file_types)
            ! Check if this variable should be written to this type of file
            select case (trim(file_types(j)))
            case ("his")
               if (.not. registered_vars(i)%to_his) cycle
               freq = registered_vars(i)%freq_his
            case ("avg")
               if (.not. registered_vars(i)%to_avg) cycle
               freq = registered_vars(i)%freq_avg
            case ("rst")
               if (.not. registered_vars(i)%to_rst) cycle
               freq = registered_vars(i)%freq_rst
            end select

            ! Skip if frequency not defined
            if (freq <= 0) cycle

            ! Generate filename
            filename = generate_filename(registered_vars(i)%file_prefix, file_types(j), freq)

            ! Check if file already exists in our list
            file_exists = .false.
            if (allocated(open_files)) then
               do file_idx = 1, size(open_files)
                  if (trim(open_files(file_idx)%filename) == trim(filename)) then
                     file_exists = .true.
                     ncid = open_files(file_idx)%ncid
                     exit
                  end if
               end do
            end if

            ! If file doesn't exist, create it
            if (.not. file_exists) then
               print *, "Creating file: ", trim(filename), " for variable: ", trim(registered_vars(i)%name)

               ! Create NetCDF file
               call nc_check(nf90_create(filename, nf90_clobber, ncid))
               print *, "File created: ", trim(filename)

               ! Add to open files list
               if (.not. allocated(open_files)) then
                  allocate (open_files(1))
                  file_idx = 1
               else
                  call add_to_open_files(filename, file_types(j), freq, ncid)
                  file_idx = size(open_files)
               end if

               ! Initialize file characteristics
               open_files(file_idx)%filename = filename
               open_files(file_idx)%type = file_types(j)
               open_files(file_idx)%freq = freq
               open_files(file_idx)%ncid = ncid
               open_files(file_idx)%time_index = 1
               print *, 'open_file ok'

               ! Create time dimension
               call nc_check(nf90_def_dim(ncid, "time", nf90_unlimited, open_files(file_idx)%time_dimid))
               print *, 'time dim ok'

               ! Create time variable if units provided
               if (present(time_units)) then
                  call nc_check(nf90_def_var(ncid, "time", nf90_real, [open_files(file_idx)%time_dimid], &
                                             open_files(file_idx)%time_varid))
                  call nc_check(nf90_put_att(ncid, open_files(file_idx)%time_varid, "units", trim(time_units)))
                  if (present(calendar)) then
                     call nc_check(nf90_put_att(ncid, open_files(file_idx)%time_varid, "calendar", trim(calendar)))
                  end if
               end if
            end if
            print *, 'time attribute ok'

            ! Define this variable in the file
            call define_single_variable(ncid, file_types(j), i)
            print *, 'define variable ok'

            ! End definition mode
            call nc_check(nf90_enddef(ncid))
         end do
      end do
   end subroutine initialize_output_files

   !> Define a single variable in a NetCDF file
   !>
   !> @param[in] ncid   NetCDF file ID
   !> @param[in] tag    File type ("his", "avg", or "rst")
   !> @param[in] var_idx Index of the variable to define
   subroutine define_single_variable(ncid, tag, var_idx)
      integer, intent(in) :: ncid
      character(len=*), intent(in) :: tag
      integer, intent(in) :: var_idx
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
   end subroutine define_single_variable

   !> Close all open output files and deallocate memory
   !>
   !> This function closes all open NetCDF files and frees memory
   !> associated with file tracking.
   subroutine close_all_output_files()
      integer :: i

      if (.not. allocated(open_files)) then
         print *, "Warning: No files to close"
         return
      end if

      do i = 1, size(open_files)
         call nc_check(nf90_close(open_files(i)%ncid))
         print *, "Closed file: ", trim(open_files(i)%filename)
      end do

      ! Free memory
      deallocate (open_files)

      if (allocated(registered_vars)) then
         do i = 1, size(registered_vars)
            call registered_vars(i)%deallocate_buffers()
         end do
      end if

   end subroutine close_all_output_files

   !> Write all outputs for the current time step
   !>
   !> @param[in] current_time  Current model time in seconds
   !> @param[in] is_final_step Optional: flag indicating if this is the final time step
   subroutine write_all_outputs(current_time, is_final_step)
      real, intent(in) :: current_time
      logical, intent(in), optional :: is_final_step
      integer :: i
      logical :: final_step

      ! Check if this is the final time step
      final_step = .false.
      if (present(is_final_step)) then
         final_step = is_final_step
      end if

      if (.not. allocated(open_files)) then
         print *, "Warning: No files open for writing"
         return
      end if

      ! Process all open files
      do i = 1, size(open_files)
         ! Handle restart files specially
         if (trim(open_files(i)%type) == "rst") then
            ! Write at defined frequency OR if this is the final step
            if (mod(current_time, open_files(i)%freq) < 1e-5 .or. final_step) then
               print *, "Writing to restart file: ", trim(open_files(i)%filename), " at time: ", current_time
               call write_output(open_files(i)%ncid, "rst", current_time)
            end if
         else
            ! History and average files - write according to normal schedule
            call write_output(open_files(i)%ncid, trim(open_files(i)%type), current_time)
         end if
      end do
   end subroutine write_all_outputs

   !> Clean up a single variable's resources
   !>
   !> @param[inout] var  Variable to clean up
   subroutine cleanup_variable(var)
      type(nc_var), intent(inout) :: var

      ! Dissociate pointers
      if (associated(var%scalar)) nullify (var%scalar)
      if (associated(var%data_1d)) nullify (var%data_1d)
      if (associated(var%data_2d)) nullify (var%data_2d)
      if (associated(var%data_3d)) nullify (var%data_3d)

      ! Deallocate averaging buffers
      if (allocated(var%avg_buffer_1d)) deallocate (var%avg_buffer_1d)
      if (allocated(var%avg_buffer_2d)) deallocate (var%avg_buffer_2d)
      if (allocated(var%avg_buffer_3d)) deallocate (var%avg_buffer_3d)
   end subroutine cleanup_variable

   !> Clean up all registered variables
   subroutine cleanup_all_variables()
      integer :: i

      if (allocated(registered_vars)) then
         do i = 1, size(registered_vars)
            call cleanup_variable(registered_vars(i))
         end do
         deallocate (registered_vars)
      end if
   end subroutine cleanup_all_variables

   !> Finalize the output system
   !>
   !> This subroutine performs all necessary cleanup operations
   !> for the output system, including closing files and freeing memory.
   subroutine finalize_output()
      ! Close all files
      call close_all_output_files()

      ! Clean up all variables
      call cleanup_all_variables()

      ! Free any other allocated memory
      ! if (allocated(dyn_vars)) deallocate(dyn_vars)

      ! Reset other global states if needed
      ! ...
   end subroutine finalize_output

end module file_manager
