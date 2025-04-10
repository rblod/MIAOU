!===============================================================================
!> @file file_manager_init.F90
!>
!> Submodule for initialization and cleanup
!>
!> This submodule handles the initialization of output files and cleanup
!> operations at program termination.
!>
!> @author Rachid Benshila
!> @date 2025-04-10
!===============================================================================
submodule(file_manager) file_manager_init
contains

   !> Initialize all output files
   !>
   !> @param[in] time_units  Optional: Time units string (e.g., "seconds since...")
   !> @param[in] calendar    Optional: Calendar type
   module procedure initialize_output_files
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
   end procedure initialize_output_files

   !> Close all open output files and deallocate memory
   !>
   !> This function closes all open NetCDF files and frees memory
   !> associated with file tracking.
   module procedure close_all_output_files
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

   end procedure close_all_output_files

   !> Clean up a single variable's resources
   !>
   !> @param[inout] var  Variable to clean up
   module procedure cleanup_variable
   ! Dissociate pointers
   if (associated(var%scalar)) nullify (var%scalar)
   if (associated(var%data_1d)) nullify (var%data_1d)
   if (associated(var%data_2d)) nullify (var%data_2d)
   if (associated(var%data_3d)) nullify (var%data_3d)

   ! Deallocate averaging buffers
   if (allocated(var%avg_buffer_1d)) deallocate (var%avg_buffer_1d)
   if (allocated(var%avg_buffer_2d)) deallocate (var%avg_buffer_2d)
   if (allocated(var%avg_buffer_3d)) deallocate (var%avg_buffer_3d)
   end procedure cleanup_variable

   !> Clean up all registered variables
   module procedure cleanup_all_variables
   integer :: i

   if (allocated(registered_vars)) then
      do i = 1, size(registered_vars)
         call cleanup_variable(registered_vars(i))
      end do
      deallocate (registered_vars)
   end if
   end procedure cleanup_all_variables

   !> Finalize the output system
   !>
   !> This subroutine performs all necessary cleanup operations
   !> for the output system, including closing files and freeing memory.
   module procedure finalize_output
   ! Close all files
   call close_all_output_files()

   ! Clean up all variables
   call cleanup_all_variables()

   ! Free any other allocated memory
   ! if (allocated(dyn_vars)) deallocate(dyn_vars)

   ! Reset other global states if needed
   ! ...
   end procedure finalize_output

end submodule file_manager_init
