!===============================================================================
!> @file file_manager_registration.F90
!>
!> Submodule for variable registration
!>
!> This submodule handles the registration of variables with the output system,
!> including memory management for variable storage.
!>
!> @author Rachid Benshila
!> @date 2025-04-10
!===============================================================================
submodule (file_manager) file_manager_registration
contains

   !> Register a variable for output with improved pre-allocation strategy
   !>
   !> This enhanced version pre-allocates space for multiple variables and
   !> reuses empty slots before growing the array.
   !>
   !> @param[in] v  Variable to register
   module procedure register_variable
      type(nc_var), allocatable :: tmp(:)
      integer :: i, n, new_size, var_index
      logical :: found_empty_slot

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
         found_empty_slot = .false.

         ! Look for an empty slot
         do i = 1, n
            if (trim(registered_vars(i)%name) == "") then
               registered_vars(i) = v
               call associate_variable_data(registered_vars(i), v)
               var_index = i
               found_empty_slot = .true.

               print *
               print *, 'FOUND EMPTY SLOT AT INDEX ', var_index, ' FOR ', v%name
               print *
               exit  ! Exit the loop but continue execution
            end if
         end do

         ! If no empty slot was found
         if (.not. found_empty_slot) then
            ! Calculate optimal new size - using golden ratio for efficient growth
            new_size = n + nint(n * 0.618) + 5  ! ~62% growth plus small constant
            
            ! Allocate new array with bigger size
            allocate(tmp(new_size))
            
            ! Copy existing data
            tmp(1:n) = registered_vars
            
            ! Add new variable
            tmp(n + 1) = v
            
            ! Mark new elements as unused
            do i = n + 2, new_size
               tmp(i)%name = ""
            end do
            
            ! Use move_alloc for efficient transfer and automatic deallocation
            call move_alloc(tmp, registered_vars)
            
            ! Associate the data pointers for the new variable
            call associate_variable_data(registered_vars(n + 1), v)
            
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
   end procedure register_variable

   !> Helper procedure to associate variable data pointers
   !>
   !> @param[inout] target_var  Target variable to receive data pointers
   !> @param[in]    source_var  Source variable with data pointers to copy
   module procedure associate_variable_data
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
   end procedure associate_variable_data

end submodule file_manager_registration