!===============================================================================
!> @file io_averaging.F90
!>
!> @brief Generic averaging module for the I/O system
!>
!> This module provides a simple interface for time averaging of variables.
!> The actual averaging logic is now encapsulated in the `avg_buffer` type
!> within `io_definitions`, and in the `io_variable` methods.
!>
!> This module provides compatibility wrappers for existing code.
!>
!> @see io_definitions.avg_buffer The underlying buffer type
!> @see io_definitions.io_variable%accumulate Method for accumulation
!>
!> @author Rachid Benshila
!> @date 2025-04
!===============================================================================
module io_averaging
   use io_definitions, only: io_variable
   implicit none
   private

   ! Public procedures (compatibility wrappers)
   public :: avg_accumulate
   public :: avg_reset
   public :: avg_init_buffers
   public :: avg_get_count
   public :: avg_is_ready

contains

   !---------------------------------------------------------------------------
   !> @brief Accumulate variable values for averaging
   !>
   !> Wrapper that calls the variable's accumulate method.
   !>
   !> @param[inout] var_ptr  Pointer to variable to accumulate
   !---------------------------------------------------------------------------
   subroutine avg_accumulate(var_ptr)
      type(io_variable), pointer, intent(inout) :: var_ptr

      if (.not. associated(var_ptr)) return
      call var_ptr%accumulate()
   end subroutine avg_accumulate

   !---------------------------------------------------------------------------
   !> @brief Reset averaging buffers for a variable
   !>
   !> @param[inout] var_ptr  Pointer to variable to reset
   !---------------------------------------------------------------------------
   subroutine avg_reset(var_ptr)
      type(io_variable), pointer, intent(inout) :: var_ptr

      if (.not. associated(var_ptr)) return
      call var_ptr%avg%reset()
   end subroutine avg_reset

   !---------------------------------------------------------------------------
   !> @brief Initialize averaging buffers for a variable
   !>
   !> @param[inout] var_ptr  Pointer to variable
   !---------------------------------------------------------------------------
   subroutine avg_init_buffers(var_ptr)
      type(io_variable), pointer, intent(inout) :: var_ptr

      if (.not. associated(var_ptr)) return
      call var_ptr%init_avg()
   end subroutine avg_init_buffers

   !---------------------------------------------------------------------------
   !> @brief Get the accumulation count
   !>
   !> @param[in] var_ptr  Pointer to variable
   !> @return    Number of accumulated samples
   !---------------------------------------------------------------------------
   function avg_get_count(var_ptr) result(count)
      type(io_variable), pointer, intent(in) :: var_ptr
      integer :: count

      if (associated(var_ptr)) then
         count = var_ptr%avg%get_count()
      else
         count = 0
      end if
   end function avg_get_count

   !---------------------------------------------------------------------------
   !> @brief Check if average is ready to be computed
   !>
   !> @param[in] var_ptr  Pointer to variable
   !> @return    True if ready
   !---------------------------------------------------------------------------
   function avg_is_ready(var_ptr) result(is_ready)
      type(io_variable), pointer, intent(in) :: var_ptr
      logical :: is_ready

      if (associated(var_ptr)) then
         is_ready = var_ptr%avg%is_ready()
      else
         is_ready = .false.
      end if
   end function avg_is_ready

end module io_averaging
