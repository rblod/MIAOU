!===============================================================================
! file_manager.F90 : Generalized Output Manager with Grid-Based Dynamic Dimensions and Unlimited Time Axis
!===============================================================================
!>
!>
!>
module file_manager
   use netcdf
   implicit none
   public :: register_variable, define_output_file, write_output, check, generate_filename

   type :: grid  !! strucutre for grid type
      character(len=16) :: name                        !! e.g. "rho", "u", "v"
      character(len=32), allocatable :: dim_names(:)   !! e.g. ["xi_rho", "eta_rho", "s_rho", "time"]
      integer, allocatable :: dim_sizes(:)             !! e.g. [nx, ny, nz, 0]
      integer, allocatable :: dim_ids(:)               !! NetCDF IDs (assigned per file)
      logical :: has_time = .false.                    ! not surre about this
   end type

   type :: nc_var  !! strucure for wring variavbes (defined in registry)
      character(len=32) :: name
      character(len=64) :: long_name
      character(len=32) :: units
      type(grid) :: var_grid
      integer :: varid_his = -1
      integer :: varid_avg = -1
      integer :: varid_rst = -1
      logical :: to_his = .false.
      logical :: to_avg = .false.
      logical :: to_rst = .false.
      real, pointer :: data(:, :) => null()
      real, allocatable :: avg_buffer(:, :)
      integer :: current_time_index = 1
      real :: freq_his = -1.        ! Fréquence pour history
      real :: freq_avg = -1.        ! Fréquence pour average
      real :: freq_rst = -1.        ! Fréquence pour restart
      character(len=128) :: file_prefix = ""
   end type

   type :: output_file  !! Structure for open files
      character(len=256) :: filename  ! Nom complet du fichier
      character(len=16) :: type       ! "his", "avg", ou "rst"
      real :: freq                    ! Fréquence d'écriture
      integer :: ncid                 ! ID NetCDF
      integer :: time_dimid           ! ID dimension temps
      integer :: time_varid           ! ID variable temps
      integer :: time_index = 1       ! Index d'écriture du temps
   end type

   type(nc_var), allocatable :: registered_vars(:)
   type(output_file), allocatable :: open_files(:)
!>
!>
!> @param var_prefix, file_type, freq
!>
!> @param var_prefix, file_type, freq

contains

   function generate_filename(var_prefix, file_type, freq) result(filename)

      use namelist_output, only: his_prefix, avg_prefix, rst_prefix
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
         ! Specific variable prefix, but still include the file type
         select case (trim(file_type))
         case ("his")
            prefix = trim(var_prefix)//"_his"
         case ("avg")
            prefix = trim(var_prefix)//"_avg"
         case ("rst")
            prefix = trim(var_prefix)//"_rst"
         case default
            prefix = trim(var_prefix)
         end select
      end if

      ! Convert freq to string
      if (freq > 0) then
         write (freq_str, '(I0)') nint(freq)
         filename = trim(prefix)//'_'//trim(freq_str)//'s.nc'
      else
         filename = trim(prefix)//'.nc'
      end if

   end function generate_filename

   function find_or_create_file(prefix, file_type, freq, time_units, calendar) result(ncid)

      character(len=*), intent(in) :: prefix, file_type
      real, intent(in) :: freq
      character(len=*), intent(in), optional :: time_units, calendar
      integer :: ncid
      character(len=256) :: filename
      integer :: i, file_idx
      logical :: file_exists

      ! File name
      filename = generate_filename(prefix, file_type, freq)

      ! It's open or not
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

      ! If not open
      if (.not. file_exists) then
         ! Create it
         call check(nf90_create(filename, nf90_clobber, ncid))

         ! Create or Add to the openfiles list
         if (.not. allocated(open_files)) then
            ! Create
            allocate (open_files(1))
            file_idx = 1
         else
            ! Add
            call add_to_open_files(filename, file_type, freq, ncid)
            file_idx = size(open_files)
         end if

         ! Initialize the new file characteristics
         open_files(file_idx)%filename = filename
         open_files(file_idx)%type = file_type
         open_files(file_idx)%freq = freq
         open_files(file_idx)%ncid = ncid
         open_files(file_idx)%time_index = 1

         ! Time axis for all file types (including restart)
         call check(nf90_def_dim(ncid, "time", nf90_unlimited, open_files(file_idx)%time_dimid))
         if (present(time_units)) then
            call check(nf90_def_var(ncid, "time", nf90_real, [open_files(file_idx)%time_dimid], &
                                    open_files(file_idx)%time_varid))
            call check(nf90_put_att(ncid, open_files(file_idx)%time_varid, "units", trim(time_units)))
            if (present(calendar)) then
               call check(nf90_put_att(ncid, open_files(file_idx)%time_varid, "calendar", trim(calendar)))
            end if
         end if
      end if

      ! File ID
      ncid = open_files(file_idx)%ncid

   end function find_or_create_file
!>
!>
!> @param filename, file_type, freq, ncid
!>
!> @param filename, file_type, freq, ncid

   subroutine add_to_open_files(filename, file_type, freq, ncid)
      character(len=*), intent(in) :: filename, file_type
      real, intent(in) :: freq
      integer, intent(in) :: ncid
      type(output_file), allocatable :: temp(:)
      integer :: n

      if (.not. allocated(open_files)) then
         allocate (open_files(1))
         open_files(1)%filename = filename
         open_files(1)%type = file_type
         open_files(1)%freq = freq
         open_files(1)%ncid = ncid
         open_files(1)%time_index = 1
      else
         n = size(open_files)
         allocate (temp(n))
         temp = open_files
         deallocate (open_files)
         allocate (open_files(n + 1))
         open_files(1:n) = temp
         open_files(n + 1)%filename = filename
         open_files(n + 1)%type = file_type
         open_files(n + 1)%freq = freq
         open_files(n + 1)%ncid = ncid
         open_files(n + 1)%time_index = 1
      end if
   end subroutine add_to_open_files
!>
!>
!> @param v
!>
!> @param v

   subroutine register_variable(v)

      type(nc_var), intent(in) :: v
      type(nc_var), allocatable :: tmp(:)
      integer :: n

      if (.not. allocated(registered_vars)) then
         allocate (registered_vars(1))
         registered_vars(1) = v
         if (associated(v%data)) then
            registered_vars(1)%data => v%data
         else
            print *, "Warning: data not associated for variable ", trim(v%name)
         end if
      else
         n = size(registered_vars)
         allocate (tmp(n))
         tmp = registered_vars
         deallocate (registered_vars)
         allocate (registered_vars(n + 1))
         registered_vars(1:n) = tmp
         registered_vars(n + 1) = v
         if (associated(v%data)) then
            registered_vars(n + 1)%data => v%data
         else
            print *, "Warning: data not associated for variable ", trim(v%name)
         end if
      end if

   end subroutine register_variable
!>
!>
!> @param ncid, tag
!>
!> @param ncid, tag

   subroutine define_output_file(ncid, tag)
      integer, intent(in) :: ncid
      character(len=*), intent(in) :: tag
      integer :: i, j, ncerr
      integer :: dim_id
      integer :: time_dimid_local

      ! Find the good file
      do i = 1, size(open_files)
         if (open_files(i)%ncid == ncid) then
            time_dimid_local = open_files(i)%time_dimid
            exit
         end if
      end do

      !
      do i = 1, size(registered_vars)
         select case (tag)
         case ("his")
            if (.not. registered_vars(i)%to_his) cycle
         case ("avg")
            if (.not. registered_vars(i)%to_avg) cycle
         case ("rst")
            if (.not. registered_vars(i)%to_rst) cycle
         end select

         ! Réinitialiser les IDs de dimension de la grille
         registered_vars(i)%var_grid%dim_ids = -1

         ! Define the grid if not already
         do j = 1, size(registered_vars(i)%var_grid%dim_names)
            ! Check if dim exist
            ncerr = nf90_inq_dimid(ncid, &
                                   trim(registered_vars(i)%var_grid%dim_names(j)), &
                                   dim_id)
            if (ncerr /= nf90_noerr) then
               ! If not create it
               ncerr = nf90_def_dim(ncid, &
                                    trim(registered_vars(i)%var_grid%dim_names(j)), &
                                    registered_vars(i)%var_grid%dim_sizes(j), &
                                    registered_vars(i)%var_grid%dim_ids(j))
               call check(ncerr)
            else
               ! If yes dind the dim ID
               registered_vars(i)%var_grid%dim_ids(j) = dim_id
            end if
         end do

         ! Variable definition
         select case (tag)
         case ("his")
            ncerr = nf90_def_var(ncid, registered_vars(i)%name, nf90_real, &
                                 [registered_vars(i)%var_grid%dim_ids(1:2), time_dimid_local], &
                                 registered_vars(i)%varid_his)
            call check(ncerr)
            ! Add attributes
            ncerr = nf90_put_att(ncid, registered_vars(i)%varid_his, "long_name", &
                                 trim(registered_vars(i)%long_name))
            call check(ncerr)
            ncerr = nf90_put_att(ncid, registered_vars(i)%varid_his, "units", &
                                 trim(registered_vars(i)%units))
            call check(ncerr)
         case ("avg")
            ncerr = nf90_def_var(ncid, registered_vars(i)%name, nf90_real, &
                                 [registered_vars(i)%var_grid%dim_ids(1:2), time_dimid_local], &
                                 registered_vars(i)%varid_avg)
            call check(ncerr)
            ! Add  attributes
            ncerr = nf90_put_att(ncid, registered_vars(i)%varid_avg, "long_name", &
                                 trim(registered_vars(i)%long_name))
            call check(ncerr)
            ncerr = nf90_put_att(ncid, registered_vars(i)%varid_avg, "units", &
                                 trim(registered_vars(i)%units))
            call check(ncerr)
            if (.not. allocated(registered_vars(i)%avg_buffer)) then
               allocate (registered_vars(i)%avg_buffer(size(registered_vars(i)%data, 1),&
               &size(registered_vars(i)%data, 2)))
            end if
         case ("rst")
            ncerr = nf90_def_var(ncid, registered_vars(i)%name, nf90_double, &  ! Changed from nf90_real to nf90_double
                                 [registered_vars(i)%var_grid%dim_ids(1:2), time_dimid_local], &  ! Added time dimension
                                 registered_vars(i)%varid_rst)
            call check(ncerr)
            ! Add attributes
            ncerr = nf90_put_att(ncid, registered_vars(i)%varid_rst, "long_name", &
                                 trim(registered_vars(i)%long_name))
            call check(ncerr)
            ncerr = nf90_put_att(ncid, registered_vars(i)%varid_rst, "units", &
                                 trim(registered_vars(i)%units))
            call check(ncerr)
         end select
      end do

   end subroutine define_output_file

!>
!>
!> @param ncid, tag, time_value
!>
!> @param ncid, tag, time_value

   subroutine write_output(ncid, tag, time_value)
      use ocean_var, only: dt  ! Import time step from ocean_var
      integer, intent(in) :: ncid
      character(len=*), intent(in) :: tag
      real, intent(in) :: time_value
      integer :: i, file_idx
      integer, dimension(3) :: start, count
      real :: time_slice(1)
      logical :: should_write, any_written
      integer :: steps_since_last_write
      real :: freq
      real, parameter :: TOL = 1.0e-5

      ! Find file index
      file_idx = -1
      do i = 1, size(open_files)
         if (open_files(i)%ncid == ncid) then
            file_idx = i
            exit
         end if
      end do

      if (file_idx == -1) then
         print *, "Error: File not found in open_files list"
         return
      end if

      any_written = .false.

      ! Store average if needed
      do i = 1, size(registered_vars)
         if (tag == "avg" .and. registered_vars(i)%to_avg) then
            ! Allocate the buffer if needed
            if (.not. allocated(registered_vars(i)%avg_buffer)) then
               allocate (registered_vars(i)%avg_buffer(size(registered_vars(i)%data, 1), &
                                                       size(registered_vars(i)%data, 2)))
               registered_vars(i)%avg_buffer = 0.0
               print *, "Initialized avg_buffer for ", trim(registered_vars(i)%name)
            end if

            ! Accumulate
            registered_vars(i)%avg_buffer = registered_vars(i)%avg_buffer + registered_vars(i)%data

            print *, "Accumulated ", trim(registered_vars(i)%name), &
               " buffer sum=", sum(registered_vars(i)%avg_buffer)
         end if
      end do

      ! Check if we need to write any variable
      do i = 1, size(registered_vars)
         should_write = .false.

         ! which file type
         select case (tag)
         case ("his")
            if (.not. registered_vars(i)%to_his) cycle
            freq = registered_vars(i)%freq_his  ! frequency history
         case ("avg")
            if (.not. registered_vars(i)%to_avg) cycle
            freq = registered_vars(i)%freq_avg  ! frequency average
         case ("rst")
            if (.not. registered_vars(i)%to_rst) cycle
            freq = registered_vars(i)%freq_rst  ! frequency restart
         end select

         ! Check the prefix matches
         if (trim(registered_vars(i)%file_prefix) /= "") then
            if (index(open_files(file_idx)%filename, trim(registered_vars(i)%file_prefix)) /= 1) cycle
         end if

         ! Determine if we should write
         if (abs(time_value) < TOL) then
            ! First time step - always write
            should_write = .true.
         else
            ! Check frequency and time
            should_write = abs(mod(time_value, freq)) < TOL
         end if

         ! Is there something to write
         if (should_write) then
            any_written = .true.
            exit
         end if
      end do

      ! If nothing to write, return
      if (.not. any_written) then
         return
      end if

      ! Here we are, we write the time value first
      ! For all file types, including restart
      if (open_files(file_idx)%time_varid /= -1) then
         time_slice(1) = time_value
         call check(nf90_put_var(ncid, open_files(file_idx)%time_varid, time_slice, &
                                 start=[open_files(file_idx)%time_index], count=[1]))
      end if

      ! Loop on variables to write each one
      do i = 1, size(registered_vars)
         should_write = .false.

         ! Same checks as above
         select case (tag)
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

         if (trim(registered_vars(i)%file_prefix) /= "") then
            if (index(open_files(file_idx)%filename, trim(registered_vars(i)%file_prefix)) /= 1) cycle
         end if

         if (abs(time_value) < TOL) then
            should_write = .true.
         else
            should_write = abs(mod(time_value, freq)) < TOL
         end if

         ! Write if needed
         if (should_write) then
            print *, "Writing ", trim(registered_vars(i)%name), " to ", trim(tag), &
               " file at time=", time_value, " index=", open_files(file_idx)%time_index

            select case (tag)
            case ("his")
               if (registered_vars(i)%varid_his > 0) then
                  start = [1, 1, open_files(file_idx)%time_index]
                  count = [size(registered_vars(i)%data, 1), size(registered_vars(i)%data, 2), 1]

                  call check(nf90_put_var(ncid, registered_vars(i)%varid_his, &
                                          registered_vars(i)%data, &
                                          start=start, count=count))
               end if

            case ("avg")
               ! Determine steps since last write for averaging
               if (abs(time_value) < TOL) then
                  steps_since_last_write = 1
               else
                  steps_since_last_write = nint(freq/dt)  ! AVG
               end if

               ! Check if there's something to write
               if (maxval(abs(registered_vars(i)%avg_buffer)) > TOL) then
                  if (registered_vars(i)%varid_avg > 0) then
                     start = [1, 1, open_files(file_idx)%time_index]
                     count = [size(registered_vars(i)%data, 1), size(registered_vars(i)%data, 2), 1]

                     print *, "Writing AVG for ", trim(registered_vars(i)%name), &
                        " steps=", steps_since_last_write, &
                        " sum=", sum(registered_vars(i)%avg_buffer), &
                        " avg=", sum(registered_vars(i)%avg_buffer)/real(steps_since_last_write)

                     ! Calculate average when writing
                     call check(nf90_put_var(ncid, registered_vars(i)%varid_avg, &
                                             registered_vars(i)%avg_buffer/real(steps_since_last_write), &
                                             start=start, count=count))
                  end if
               else
                  print *, "Warning: Empty avg_buffer for ", trim(registered_vars(i)%name)
               end if

               ! Reset buffer for next average period
               registered_vars(i)%avg_buffer = 0.0

            case ("rst")
               if (registered_vars(i)%varid_rst > 0) then
                  start = [1, 1, open_files(file_idx)%time_index]
                  count = [size(registered_vars(i)%data, 1), size(registered_vars(i)%data, 2), 1]

                  ! Explicit conversion to double precision for restart
                  call check(nf90_put_var(ncid, registered_vars(i)%varid_rst, &
                                          dble(registered_vars(i)%data), &
                                          start=start, count=count))
               end if
            end select
         end if
      end do

      ! Increment time index for all file types when any variable was written
      if (any_written) then
         open_files(file_idx)%time_index = open_files(file_idx)%time_index + 1
         print *, "Incremented time index to ", open_files(file_idx)%time_index, " for file ", &
            trim(open_files(file_idx)%filename)
      end if
   end subroutine write_output
!>
!>
!> @param time_units, calendar
!>
!> @param time_units, calendar

! Refactored version of initialize_output_files
! The issue is in how we handle the individual file prefixes for variables.
! Let's modify the initialize_output_files subroutine to properly handle
! custom prefixes for each variable and file type:

   subroutine initialize_output_files(time_units, calendar)
      character(len=*), intent(in), optional :: time_units, calendar
      integer :: i, j, ncid
      real :: freq
      logical :: file_needed
      character(len=16) :: file_types(3) = ["his", "avg", "rst"]

      ! Check if variables are registered
      if (.not. allocated(registered_vars)) then
         print *, "Warning: No variables registered before initializing output files"
         return
      end if

      ! Loop through all file types
      do j = 1, size(file_types)
         ! First, handle global files (variables with empty file_prefix)
         ! Check if any variable needs this file type with global prefix
         file_needed = .false.
         do i = 1, size(registered_vars)
            select case (trim(file_types(j)))
            case ("his")
               if (registered_vars(i)%to_his .and. trim(registered_vars(i)%file_prefix) == "") then
                  file_needed = .true.
                  freq = registered_vars(i)%freq_his
                  exit
               end if
            case ("avg")
               if (registered_vars(i)%to_avg .and. trim(registered_vars(i)%file_prefix) == "") then
                  file_needed = .true.
                  freq = registered_vars(i)%freq_avg
                  exit
               end if
            case ("rst")
               if (registered_vars(i)%to_rst .and. trim(registered_vars(i)%file_prefix) == "") then
                  file_needed = .true.
                  freq = registered_vars(i)%freq_rst
                  exit
               end if
            end select
         end do

         ! Create global file if needed
         if (file_needed) then
            ncid = find_or_create_file("", trim(file_types(j)), freq, time_units, calendar)
            call define_output_file(ncid, trim(file_types(j)))
            call check(nf90_enddef(ncid))
         end if

         ! Now handle specific files for each variable with custom file_prefix
         do i = 1, size(registered_vars)
            ! Skip if this variable doesn't use a custom prefix
            if (trim(registered_vars(i)%file_prefix) == "") cycle

            ! Check if this file type is needed for this variable
            file_needed = .false.
            select case (trim(file_types(j)))
            case ("his")
               if (registered_vars(i)%to_his) then
                  file_needed = .true.
                  freq = registered_vars(i)%freq_his
               end if
            case ("avg")
               if (registered_vars(i)%to_avg) then
                  file_needed = .true.
                  freq = registered_vars(i)%freq_avg
               end if
            case ("rst")
               if (registered_vars(i)%to_rst) then
                  file_needed = .true.
                  freq = registered_vars(i)%freq_rst
               end if
            end select

            ! If needed, create the custom file for this variable
            if (file_needed) then
               print *, "Creating custom file for variable ", trim(registered_vars(i)%name), &
                  " with prefix ", trim(registered_vars(i)%file_prefix), &
                  " and type ", trim(file_types(j))

               ncid = find_or_create_file(trim(registered_vars(i)%file_prefix), &
                                          trim(file_types(j)), freq, time_units, calendar)
               call define_output_file(ncid, trim(file_types(j)))
               call check(nf90_enddef(ncid))
            end if
         end do
      end do
   end subroutine initialize_output_files
!>
!> @param
!>
!> @param

   subroutine close_all_output_files()
      integer :: i

      if (.not. allocated(open_files)) then
         print *, "Warning: No files to close"
         return
      end if

      do i = 1, size(open_files)
         call check(nf90_close(open_files(i)%ncid))
         print *, "Closed file: ", trim(open_files(i)%filename)
      end do

      !
      deallocate (open_files)
   end subroutine close_all_output_files
!>
!>
!> @param current_time
!>
!> @param current_time

   subroutine write_all_outputs(current_time, is_final_step)
      real, intent(in) :: current_time
      logical, intent(in), optional :: is_final_step
      integer :: i
      logical :: final_step

      ! Determine if this is the final step
      final_step = .false.
      if (present(is_final_step)) then
         final_step = is_final_step
      end if

      if (.not. allocated(open_files)) then
         print *, "Warning: No files open for writing"
         return
      end if

      do i = 1, size(open_files)
         ! Restart case
         if (trim(open_files(i)%type) == "rst") then
            ! Write at the defined frequency OR if it's the last step
            if (mod(current_time, open_files(i)%freq) < 1e-5 .or. final_step) then
               call write_output(open_files(i)%ncid, "rst", current_time)
            end if
         else
            ! History and average files
            call write_output(open_files(i)%ncid, trim(open_files(i)%type), current_time)
         end if
      end do
   end subroutine write_all_outputs

!>
!>
!> @param ierr
!>
!> @param ierr

   subroutine check(ierr)
      integer, intent(in) :: ierr
      if (ierr /= nf90_noerr) then
         print *, 'NetCDF error:', trim(nf90_strerror(ierr))
         stop
      end if
   end subroutine check

end module file_manager
