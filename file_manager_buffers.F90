!===============================================================================
!> @file file_manager_buffers.F90
!>
!> Submodule for managing variable buffers
!>
!> This submodule handles allocation, deallocation, and reset of variable
!> averaging buffers used in the file_manager module.
!>
!> @author Rachid Benshila
!> @date 2025-04-10
!===============================================================================
submodule(file_manager) file_manager_buffers
contains

   !> Allocate averaging buffers if needed
   !>
   !> @param[inout] this  Variable to allocate buffers for
   module procedure allocate_buffers
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
   end procedure allocate_buffers

   !> Deallocate averaging buffers if the variable owns them
   !>
   !> @param[inout] this  Variable to deallocate buffers for
   module procedure deallocate_buffers
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
   end procedure deallocate_buffers

   !> Reset averaging buffers to zero without deallocating
   !>
   !> @param[inout] this  Variable to reset buffers for
   module procedure reset_avg_buffers
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
   end procedure reset_avg_buffers

end submodule file_manager_buffers
