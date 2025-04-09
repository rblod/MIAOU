!===============================================================================
! file_manager.F90 : Enhanced output manager supporting multiple dimensions (0D, 1D, 2D, 3D)
!===============================================================================
module file_manager
   use netcdf
   use grid_module
   use namelist_output, only: his_prefix, avg_prefix, rst_prefix
   use netcdf_backend, only: nc_check, nc_define_variable, nc_write_variable
   
   implicit none
   
   public :: register_variable, define_output_file, write_output, generate_filename
   public :: initialize_output_files, close_all_output_files, write_all_outputs
   
   ! Enhanced nc_var type supporting multiple dimensions
   type :: nc_var
      character(len=32) :: name
      character(len=64) :: long_name
      character(len=32) :: units
      type(grid) :: var_grid
      
      ! Dimension count (0=scalar, 1=1D, 2=2D, 3=3D)
      integer :: ndims = 0
      
      ! NetCDF variable IDs
      integer :: varid_his = -1
      integer :: varid_avg = -1
      integer :: varid_rst = -1
      
      ! Output flags
      logical :: to_his = .false.
      logical :: to_avg = .false.
      logical :: to_rst = .false.
      
      ! Data pointers for different dimensions - only one will be associated
      real, pointer :: scalar => null()             ! 0D data
      real, pointer :: data_1d(:) => null()         ! 1D data
      real, pointer :: data_2d(:,:) => null()       ! 2D data (current implementation)
      real, pointer :: data_3d(:,:,:) => null()     ! 3D data
      
      ! Averaging buffers with matching dimension
      real :: scalar_avg = 0.0                     ! For 0D
      real, allocatable :: avg_buffer_1d(:)         ! For 1D
      real, allocatable :: avg_buffer_2d(:,:)       ! For 2D (current)
      real, allocatable :: avg_buffer_3d(:,:,:)     ! For 3D
      
      ! Time and frequency settings
      integer :: current_time_index = 1
      real :: freq_his = -1.        ! Frequency for history
      real :: freq_avg = -1.        ! Frequency for average
      real :: freq_rst = -1.        ! Frequency for restart
      character(len=128) :: file_prefix = ""
   end type nc_var
   
   ! Structure for open files
   type :: output_file
      character(len=256) :: filename  ! Complete filename
      character(len=16) :: type       ! "his", "avg", or "rst"
      real :: freq                    ! Write frequency
      integer :: ncid                 ! NetCDF ID
      integer :: time_dimid           ! Time dimension ID
      integer :: time_varid           ! Time variable ID
      integer :: time_index = 1       ! Time write index
   end type output_file
   
   ! Global arrays
   type(nc_var), allocatable :: registered_vars(:)
   type(output_file), allocatable :: open_files(:)
   
contains

   !---------------------------------------------------------------------------
   ! Filename generation
   !---------------------------------------------------------------------------
   
   function generate_filename(var_prefix, file_type, freq) result(filename)
      character(len=*), intent(in) :: var_prefix, file_type
      real, intent(in) :: freq
      character(len=256) :: filename
      character(len=16) :: freq_str
      character(len=128) :: prefix

      ! Determine the base prefix
      if (trim(var_prefix) == "") then
         ! Global from namelist
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
         ! Specific variable prefix
         prefix = trim(var_prefix)
      end if

      ! Convert freq to string
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
   
   ! Find or create a file for output
   function find_or_create_file(prefix, file_type, freq, time_units, calendar) result(ncid)
      character(len=*), intent(in) :: prefix, file_type
      real, intent(in) :: freq
      character(len=*), intent(in), optional :: time_units, calendar
      integer :: ncid
      character(len=256) :: filename
      integer :: i, file_idx
      logical :: file_exists

      ! Generate filename
      filename = generate_filename(prefix, file_type, freq)

      ! Check if file already exists in our list
      file_exists = .false.
      file_idx = -1
      if (allocated(open_files)) then
         do i = 1, size(open_files)
            if (trim(open_files(i)%filename) == trim(filename)) then
               file_exists = .true.
               file_idx = i
               ncid = open_files(i)%ncid
               exit
            end if
         end do
      end if

      ! If not already open, create it
      if (.not. file_exists) then
         ! Create new NetCDF file
         call nc_check(nf90_create(filename, nf90_clobber, ncid))

         ! Add to the open files list
         if (.not. allocated(open_files)) then
            ! First file - create array
            allocate (open_files(1))
            file_idx = 1
         else
            ! Add to existing array
            call add_to_open_files(filename, file_type, freq, ncid)
            file_idx = size(open_files)
         end if

         ! Initialize the file characteristics
         open_files(file_idx)%filename = filename
         open_files(file_idx)%type = file_type
         open_files(file_idx)%freq = freq
         open_files(file_idx)%ncid = ncid
         open_files(file_idx)%time_index = 1

         ! Create time dimension for all file types
         call nc_check(nf90_def_dim(ncid, "time", nf90_unlimited, open_files(file_idx)%time_dimid))
         
         ! Add time variable if units provided
         if (present(time_units)) then
            call nc_check(nf90_def_var(ncid, "time", nf90_real, [open_files(file_idx)%time_dimid], &
                                      open_files(file_idx)%time_varid))
            call nc_check(nf90_put_att(ncid, open_files(file_idx)%time_varid, "units", trim(time_units)))
            if (present(calendar)) then
               call nc_check(nf90_put_att(ncid, open_files(file_idx)%time_varid, "calendar", trim(calendar)))
            end if
         end if
      end if

      ! Return the file ID
      ncid = open_files(file_idx)%ncid
   end function find_or_create_file
   
   ! Add a file to the open_files array
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
   
   ! Register a variable for output
   subroutine register_variable(v)
      type(nc_var), intent(in) :: v
      type(nc_var), allocatable :: tmp(:)
      integer :: n

      if (.not. allocated(registered_vars)) then
         ! First variable
         allocate (registered_vars(1))
         registered_vars(1) = v
         
         ! Associate the correct pointer based on dimension
         select case (v%ndims)
         case (0)
            if (associated(v%scalar)) then
               registered_vars(1)%scalar => v%scalar
            else
               print *, "Warning: scalar not associated for variable ", trim(v%name)
            end if
         case (1)
            if (associated(v%data_1d)) then
               registered_vars(1)%data_1d => v%data_1d
            else
               print *, "Warning: 1D data not associated for variable ", trim(v%name)
            end if
         case (2)
            if (associated(v%data_2d)) then
               registered_vars(1)%data_2d => v%data_2d
            else
               print *, "Warning: 2D data not associated for variable ", trim(v%name)
            end if
         case (3)
            if (associated(v%data_3d)) then
               registered_vars(1)%data_3d => v%data_3d
            else
               print *, "Warning: 3D data not associated for variable ", trim(v%name)
            end if
         end select
      else
         ! Add to existing array
         n = size(registered_vars)
         allocate (tmp(n))
         tmp = registered_vars
         deallocate (registered_vars)
         allocate (registered_vars(n + 1))
         registered_vars(1:n) = tmp
         registered_vars(n + 1) = v
         
         ! Associate the correct pointer based on dimension
         select case (v%ndims)
         case (0)
            if (associated(v%scalar)) then
               registered_vars(n + 1)%scalar => v%scalar
            else
               print *, "Warning: scalar not associated for variable ", trim(v%name)
            end if
         case (1)
            if (associated(v%data_1d)) then
               registered_vars(n + 1)%data_1d => v%data_1d
            else
               print *, "Warning: 1D data not associated for variable ", trim(v%name)
            end if
         case (2)
            if (associated(v%data_2d)) then
               registered_vars(n + 1)%data_2d => v%data_2d
            else
               print *, "Warning: 2D data not associated for variable ", trim(v%name)
            end if
         case (3)
            if (associated(v%data_3d)) then
               registered_vars(n + 1)%data_3d => v%data_3d
            else
               print *, "Warning: 3D data not associated for variable ", trim(v%name)
            end if
         end select
      end if

      write(*,*) 'end register_variable'
   end subroutine register_variable
   
   !---------------------------------------------------------------------------
   ! Variable definition
   !---------------------------------------------------------------------------
   
   ! Define variables in a NetCDF file
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
                     allocate(registered_vars(i)%avg_buffer_1d(size(registered_vars(i)%data_1d)))
                     registered_vars(i)%avg_buffer_1d = 0.0
                  end if
               case (2)
                  if (.not. allocated(registered_vars(i)%avg_buffer_2d)) then
                     allocate(registered_vars(i)%avg_buffer_2d( &
                              size(registered_vars(i)%data_2d, 1), &
                              size(registered_vars(i)%data_2d, 2)))
                     registered_vars(i)%avg_buffer_2d = 0.0
                  end if
               case (3)
                  if (.not. allocated(registered_vars(i)%avg_buffer_3d)) then
                     allocate(registered_vars(i)%avg_buffer_3d( &
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
                  allocate(registered_vars(i)%avg_buffer_1d(size(registered_vars(i)%data_1d)))
                  registered_vars(i)%avg_buffer_1d = 0.0
               end if
            case (2)
               if (.not. allocated(registered_vars(i)%avg_buffer_2d)) then
                  allocate(registered_vars(i)%avg_buffer_2d( &
                           size(registered_vars(i)%data_2d, 1), &
                           size(registered_vars(i)%data_2d, 2)))
                  registered_vars(i)%avg_buffer_2d = 0.0
               end if
            case (3)
               if (.not. allocated(registered_vars(i)%avg_buffer_3d)) then
                  allocate(registered_vars(i)%avg_buffer_3d( &
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
   
   ! Write data to output files
   ! Write data to output files
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
                  if (.not. allocated(registered_vars(i)%avg_buffer_1d)) then
                     allocate(registered_vars(i)%avg_buffer_1d(size(registered_vars(i)%data_1d)))
                     registered_vars(i)%avg_buffer_1d = 0.0
                  end if
                  registered_vars(i)%avg_buffer_1d = registered_vars(i)%avg_buffer_1d + &
                                                 registered_vars(i)%data_1d
               end if
            case (2)
               if (associated(registered_vars(i)%data_2d)) then
                  if (.not. allocated(registered_vars(i)%avg_buffer_2d)) then
                     allocate(registered_vars(i)%avg_buffer_2d( &
                              size(registered_vars(i)%data_2d, 1), &
                              size(registered_vars(i)%data_2d, 2)))
                     registered_vars(i)%avg_buffer_2d = 0.0
                  end if
                  registered_vars(i)%avg_buffer_2d = registered_vars(i)%avg_buffer_2d + &
                                                 registered_vars(i)%data_2d
               end if
            case (3)
               if (associated(registered_vars(i)%data_3d)) then
                  if (.not. allocated(registered_vars(i)%avg_buffer_3d)) then
                     allocate(registered_vars(i)%avg_buffer_3d( &
                              size(registered_vars(i)%data_3d, 1), &
                              size(registered_vars(i)%data_3d, 2), &
                              size(registered_vars(i)%data_3d, 3)))
                     registered_vars(i)%avg_buffer_3d = 0.0
                  end if
                  registered_vars(i)%avg_buffer_3d = registered_vars(i)%avg_buffer_3d + &
                                                 registered_vars(i)%data_3d
               end if
            end select
         end if
         
! Déterminer si nous devons écrire à ce pas de temps
if (abs(time_value) < TOL) then
   ! Premier pas de temps
   if (tag == "his") then
      ! Pour fichiers "his" - toujours écrire
      should_write = .true.
   else
      ! Pour fichiers "avg" et "rst" - ne pas écrire au temps 0
      should_write = .false.
   end if
else
   ! Autres pas de temps - vérifier la fréquence
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
! Déterminer si nous devons écrire à ce pas de temps
if (abs(time_value) < TOL) then
   ! Premier pas de temps
   if (tag == "his") then
      ! Pour fichiers "his" - toujours écrire
      should_write = .true.
   else
      ! Pour fichiers "avg" et "rst" - ne pas écrire au temps 0
      should_write = .false.
   end if
else
   ! Autres pas de temps - vérifier la fréquence
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
                                         registered_vars(i)%scalar_avg / real(steps_since_last_write), &
                                         open_files(file_idx)%time_index)
                     registered_vars(i)%scalar_avg = 0.0  ! Reset after writing
                  end if
               case (1)
                  if (allocated(registered_vars(i)%avg_buffer_1d)) then
                     if (maxval(abs(registered_vars(i)%avg_buffer_1d)) > TOL) then
                        call nc_write_variable(ncid, varid, &
                                            registered_vars(i)%avg_buffer_1d / real(steps_since_last_write), &
                                            open_files(file_idx)%time_index)
                        registered_vars(i)%avg_buffer_1d = 0.0  ! Reset after writing
                     end if
                  end if
               case (2)
                  if (allocated(registered_vars(i)%avg_buffer_2d)) then
                     if (maxval(abs(registered_vars(i)%avg_buffer_2d)) > TOL) then
                        call nc_write_variable(ncid, varid, &
                                            registered_vars(i)%avg_buffer_2d / real(steps_since_last_write), &
                                            open_files(file_idx)%time_index)
                        registered_vars(i)%avg_buffer_2d = 0.0  ! Reset after writing
                     end if
                  end if
               case (3)
                  if (allocated(registered_vars(i)%avg_buffer_3d)) then
                     if (maxval(abs(registered_vars(i)%avg_buffer_3d)) > TOL) then
                        call nc_write_variable(ncid, varid, &
                                            registered_vars(i)%avg_buffer_3d / real(steps_since_last_write), &
                                            open_files(file_idx)%time_index)
                        registered_vars(i)%avg_buffer_3d = 0.0  ! Reset after writing
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
   
   ! Initialize all output files
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
   
   ! Define a single variable in a file
! Define a single variable in a file
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
   
   ! Close all open output files
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
   end subroutine close_all_output_files
   
   ! Write all outputs for the current time step
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
end module file_manager