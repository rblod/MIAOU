!===============================================================================
!> @file io_averaging.F90
!>
!> @brief Generic averaging module for the I/O system
!>
!> This module provides backend-independent functionality for time averaging
!> of output variables. It handles buffer initialization, accumulation, and
!> average computation without any knowledge of the output format.
!>
!> The actual writing of averaged data is delegated to backend-specific
!> modules (e.g., io_netcdf_avg).
!>
!> ## Usage
!>
!> ```fortran
!> ! Initialize buffers (called automatically on first accumulation)
!> call avg_init_buffers(var_ptr)
!>
!> ! Accumulate values at each time step
!> call avg_accumulate(var_ptr)
!>
!> ! Get computed average into a temporary array
!> call avg_compute(var_ptr, avg_data_2d)
!>
!> ! Reset after writing
!> call avg_reset(var_ptr)
!> ```
!>
!> @author Rachid Benshila
!> @date 2025-04
!===============================================================================
module io_averaging
   use io_definitions, only: io_variable
   implicit none
   private

   !---------------------------------------------------------------------------
   ! Public interface
   !---------------------------------------------------------------------------
   
   public :: avg_init_buffers
   public :: avg_accumulate
   public :: avg_reset
   public :: avg_get_count
   public :: avg_is_ready
   public :: avg_compute_scalar
   public :: avg_compute_1d
   public :: avg_compute_2d
   public :: avg_compute_3d

contains

   !---------------------------------------------------------------------------
   !> @brief Initialize averaging buffers for a variable
   !>
   !> Allocates and initializes the averaging buffers for a variable based on
   !> its dimensionality. This is called automatically on first accumulation
   !> but can be called explicitly for pre-allocation.
   !>
   !> @param[inout] var_ptr  Pointer to variable to initialize
   !---------------------------------------------------------------------------
   subroutine avg_init_buffers(var_ptr)
      type(io_variable), pointer, intent(inout) :: var_ptr

      ! Safety check
      if (.not. associated(var_ptr)) return

      ! Return immediately if already initialized
      if (var_ptr%avg_initialized) return

      ! Allocate appropriate buffer based on dimensionality
      select case (var_ptr%ndims)
      case (0)  ! Scalar
         var_ptr%scalar_avg = 0.0

      case (1)  ! 1D
         if (associated(var_ptr%data_1d)) then
            if (allocated(var_ptr%data_avg_1d)) deallocate(var_ptr%data_avg_1d)
            allocate(var_ptr%data_avg_1d(size(var_ptr%data_1d)))
            var_ptr%data_avg_1d = 0.0
         end if

      case (2)  ! 2D
         if (associated(var_ptr%data_2d)) then
            if (allocated(var_ptr%data_avg_2d)) deallocate(var_ptr%data_avg_2d)
            allocate(var_ptr%data_avg_2d(size(var_ptr%data_2d, 1), &
                                          size(var_ptr%data_2d, 2)))
            var_ptr%data_avg_2d = 0.0
         end if

      case (3)  ! 3D
         if (associated(var_ptr%data_3d)) then
            if (allocated(var_ptr%data_avg_3d)) deallocate(var_ptr%data_avg_3d)
            allocate(var_ptr%data_avg_3d(size(var_ptr%data_3d, 1), &
                                          size(var_ptr%data_3d, 2), &
                                          size(var_ptr%data_3d, 3)))
            var_ptr%data_avg_3d = 0.0
         end if
      end select

      var_ptr%avg_count = 0
      var_ptr%avg_initialized = .true.
   end subroutine avg_init_buffers

   !---------------------------------------------------------------------------
   !> @brief Accumulate variable values for averaging
   !>
   !> Adds the current value of a variable to its average accumulator.
   !> The variable is modified in place through the pointer.
   !> Buffers are automatically initialized if needed.
   !>
   !> @param[inout] var_ptr  Pointer to variable to accumulate
   !---------------------------------------------------------------------------
   subroutine avg_accumulate(var_ptr)
      type(io_variable), pointer, intent(inout) :: var_ptr

      ! Safety check
      if (.not. associated(var_ptr)) return

      ! Skip if averaging not required for this variable
      if (.not. var_ptr%needs_averaging()) return

      ! Initialize buffers if needed
      call avg_init_buffers(var_ptr)

      ! Accumulate based on dimensionality
      select case (var_ptr%ndims)
      case (0)  ! Scalar
         if (associated(var_ptr%scalar)) then
            var_ptr%scalar_avg = var_ptr%scalar_avg + var_ptr%scalar
         end if

      case (1)  ! 1D
         if (associated(var_ptr%data_1d) .and. allocated(var_ptr%data_avg_1d)) then
            var_ptr%data_avg_1d = var_ptr%data_avg_1d + var_ptr%data_1d
         end if

      case (2)  ! 2D
         if (associated(var_ptr%data_2d) .and. allocated(var_ptr%data_avg_2d)) then
            var_ptr%data_avg_2d = var_ptr%data_avg_2d + var_ptr%data_2d
         end if

      case (3)  ! 3D
         if (associated(var_ptr%data_3d) .and. allocated(var_ptr%data_avg_3d)) then
            var_ptr%data_avg_3d = var_ptr%data_avg_3d + var_ptr%data_3d
         end if
      end select

      var_ptr%avg_count = var_ptr%avg_count + 1
   end subroutine avg_accumulate

   !---------------------------------------------------------------------------
   !> @brief Reset averaging buffers for a variable
   !>
   !> Clears the averaging buffers and resets the accumulation counter.
   !> Typically called after writing an average to prepare for next period.
   !>
   !> @param[inout] var_ptr  Pointer to variable to reset
   !---------------------------------------------------------------------------
   subroutine avg_reset(var_ptr)
      type(io_variable), pointer, intent(inout) :: var_ptr

      ! Safety check
      if (.not. associated(var_ptr)) return

      ! Ensure buffers exist
      call avg_init_buffers(var_ptr)

      ! Reset buffer based on dimensionality
      select case (var_ptr%ndims)
      case (0)  ! Scalar
         var_ptr%scalar_avg = 0.0

      case (1)  ! 1D
         if (allocated(var_ptr%data_avg_1d)) var_ptr%data_avg_1d = 0.0

      case (2)  ! 2D
         if (allocated(var_ptr%data_avg_2d)) var_ptr%data_avg_2d = 0.0

      case (3)  ! 3D
         if (allocated(var_ptr%data_avg_3d)) var_ptr%data_avg_3d = 0.0
      end select

      var_ptr%avg_count = 0
   end subroutine avg_reset

   !---------------------------------------------------------------------------
   !> @brief Get the accumulation count for a variable
   !>
   !> @param[in] var_ptr  Pointer to variable
   !> @return    Number of values accumulated (0 if not initialized)
   !---------------------------------------------------------------------------
   function avg_get_count(var_ptr) result(count)
      type(io_variable), pointer, intent(in) :: var_ptr
      integer :: count

      if (associated(var_ptr)) then
         count = var_ptr%avg_count
      else
         count = 0
      end if
   end function avg_get_count

   !---------------------------------------------------------------------------
   !> @brief Check if average is ready to be written
   !>
   !> Returns true if the variable has accumulated data ready for output.
   !>
   !> @param[in] var_ptr  Pointer to variable
   !> @return    True if avg_count > 0 and buffers are initialized
   !---------------------------------------------------------------------------
   function avg_is_ready(var_ptr) result(is_ready)
      type(io_variable), pointer, intent(in) :: var_ptr
      logical :: is_ready

      is_ready = .false.
      if (.not. associated(var_ptr)) return
      is_ready = (var_ptr%avg_initialized .and. var_ptr%avg_count > 0)
   end function avg_is_ready

   !---------------------------------------------------------------------------
   !> @brief Compute and return the scalar average
   !>
   !> @param[in]  var_ptr    Pointer to variable
   !> @param[out] avg_value  Computed average value
   !> @return     True if successful, false if no data accumulated
   !---------------------------------------------------------------------------
   function avg_compute_scalar(var_ptr, avg_value) result(success)
      type(io_variable), pointer, intent(in) :: var_ptr
      real, intent(out) :: avg_value
      logical :: success

      success = .false.
      avg_value = 0.0

      if (.not. associated(var_ptr)) return
      if (var_ptr%avg_count == 0) return
      if (var_ptr%ndims /= 0) return

      avg_value = var_ptr%scalar_avg / real(var_ptr%avg_count)
      success = .true.
   end function avg_compute_scalar

   !---------------------------------------------------------------------------
   !> @brief Compute and return the 1D average
   !>
   !> @param[in]  var_ptr   Pointer to variable
   !> @param[out] avg_data  Computed average array (must be pre-allocated)
   !> @return     True if successful, false if no data accumulated
   !---------------------------------------------------------------------------
   function avg_compute_1d(var_ptr, avg_data) result(success)
      type(io_variable), pointer, intent(in) :: var_ptr
      real, intent(out) :: avg_data(:)
      logical :: success

      success = .false.

      if (.not. associated(var_ptr)) return
      if (var_ptr%avg_count == 0) return
      if (var_ptr%ndims /= 1) return
      if (.not. allocated(var_ptr%data_avg_1d)) return

      avg_data = var_ptr%data_avg_1d / real(var_ptr%avg_count)
      success = .true.
   end function avg_compute_1d

   !---------------------------------------------------------------------------
   !> @brief Compute and return the 2D average
   !>
   !> @param[in]  var_ptr   Pointer to variable
   !> @param[out] avg_data  Computed average array (must be pre-allocated)
   !> @return     True if successful, false if no data accumulated
   !---------------------------------------------------------------------------
   function avg_compute_2d(var_ptr, avg_data) result(success)
      type(io_variable), pointer, intent(in) :: var_ptr
      real, intent(out) :: avg_data(:,:)
      logical :: success

      success = .false.

      if (.not. associated(var_ptr)) return
      if (var_ptr%avg_count == 0) return
      if (var_ptr%ndims /= 2) return
      if (.not. allocated(var_ptr%data_avg_2d)) return

      avg_data = var_ptr%data_avg_2d / real(var_ptr%avg_count)
      success = .true.
   end function avg_compute_2d

   !---------------------------------------------------------------------------
   !> @brief Compute and return the 3D average
   !>
   !> @param[in]  var_ptr   Pointer to variable
   !> @param[out] avg_data  Computed average array (must be pre-allocated)
   !> @return     True if successful, false if no data accumulated
   !---------------------------------------------------------------------------
   function avg_compute_3d(var_ptr, avg_data) result(success)
      type(io_variable), pointer, intent(in) :: var_ptr
      real, intent(out) :: avg_data(:,:,:)
      logical :: success

      success = .false.

      if (.not. associated(var_ptr)) return
      if (var_ptr%avg_count == 0) return
      if (var_ptr%ndims /= 3) return
      if (.not. allocated(var_ptr%data_avg_3d)) return

      avg_data = var_ptr%data_avg_3d / real(var_ptr%avg_count)
      success = .true.
   end function avg_compute_3d

end module io_averaging
