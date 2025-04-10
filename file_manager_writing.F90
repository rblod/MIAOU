!===============================================================================
!> @file file_manager_writing.F90
!>
!> Submodule for writing operations
!>
!> This submodule handles writing data to NetCDF files, including time stepping
!> and averaging.
!>
!> @author Rachid Benshila
!> @date 2025-04-10
!===============================================================================
submodule (file_manager) file_manager_writing
   use ocean_var, only: dt  ! Import time step from ocean_var
contains

   !> Write data to output files
   !>
   !> @param[in] ncid        NetCDF file ID
   !> @param[in] tag         File type ("his", "avg", or "rst")
   !> @param[in] time_value  Current model time in seconds
   module procedure write_output
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
   end procedure write_output

   !> Write all outputs for the current time step
   !>
   !> @param[in] current_time  Current model time in seconds
   !> @param[in] is_final_step Optional: flag indicating if this is the final time step
   module procedure write_all_outputs
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
   end procedure write_all_outputs

end submodule file_manager_writing