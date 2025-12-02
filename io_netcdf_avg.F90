!===============================================================================
!> @file io_netcdf_avg.F90
!>
!> NetCDF averaging implementation
!>
!> This module provides NetCDF-specific implementations for time averaging
!> operations used in the output system.
!>
!> REFACTORED: Now works with pointers to io_variable for proper state
!> modification (avg_count, buffers).
!>
!> @author Rachid Benshila
!> @date 2025-04-11
!===============================================================================
module io_netcdf_avg
   use io_definitions, only: io_variable
   use netcdf_backend, only: nc_write_variable

   implicit none
   private

   ! Public procedures
   public :: accumulate_avg, write_variable_avg, reset_avg

contains

   !> Initialize averaging buffers for a variable
   !>
   !> This subroutine allocates and initializes the averaging buffers
   !> for a variable based on its dimensionality.
   !>
   !> @param[inout] var_ptr  Pointer to variable to initialize
   subroutine init_avg_buffers(var_ptr)
      type(io_variable), pointer, intent(inout) :: var_ptr

      ! Safety check
      if (.not. associated(var_ptr)) return

      ! Return immediately if already initialized
      if (var_ptr%avg_initialized) return

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
   end subroutine init_avg_buffers

   !> Accumulate variable values for averaging
   !>
   !> This subroutine adds the current value of a variable to its average
   !> accumulator. The variable is modified in place through the pointer.
   !>
   !> @param[inout] var_ptr  Pointer to variable to accumulate
   subroutine accumulate_avg(var_ptr)
      type(io_variable), pointer, intent(inout) :: var_ptr

      ! Safety check
      if (.not. associated(var_ptr)) return

      ! Skip if averaging not required
      if (.not. var_ptr%to_avg) return

      ! Initialize buffers if needed
      call init_avg_buffers(var_ptr)

      ! Accumulate based on dimensions
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
   end subroutine accumulate_avg

   !> Write average variable data to a NetCDF file
   !>
   !> This function calculates the average value from accumulated data
   !> and writes it to the specified variable in a NetCDF file.
   !>
   !> @param[inout] var_ptr    Pointer to variable containing accumulated data
   !> @param[in]    ncid       NetCDF file ID
   !> @param[in]    varid      Variable ID in the file
   !> @param[in]    time_index Time index to write at
   !> @return       Status code (0 = success)
   function write_variable_avg(var_ptr, ncid, varid, time_index) result(status)
      type(io_variable), pointer, intent(inout) :: var_ptr
      integer, intent(in) :: ncid, varid, time_index
      integer :: status

      real :: avg_factor

      status = -1

      ! Safety check
      if (.not. associated(var_ptr)) return

      ! Ensure buffers are initialized
      call init_avg_buffers(var_ptr)

      ! Nothing to write if no accumulation
      if (var_ptr%avg_count == 0) then
         print *, "Warning: No data accumulated for averaging: ", trim(var_ptr%name)
         return
      end if

      avg_factor = 1.0 / real(var_ptr%avg_count)

      ! Calculate average and write
      select case (var_ptr%ndims)
      case (0)  ! Scalar
         call nc_write_variable(ncid, varid, var_ptr%scalar_avg * avg_factor, time_index)
         status = 0

      case (1)  ! 1D
         if (allocated(var_ptr%data_avg_1d)) then
            call nc_write_variable(ncid, varid, var_ptr%data_avg_1d * avg_factor, time_index)
            status = 0
         end if

      case (2)  ! 2D
         if (allocated(var_ptr%data_avg_2d)) then
            call nc_write_variable(ncid, varid, var_ptr%data_avg_2d * avg_factor, time_index)
            status = 0
         end if

      case (3)  ! 3D
         if (allocated(var_ptr%data_avg_3d)) then
            call nc_write_variable(ncid, varid, var_ptr%data_avg_3d * avg_factor, time_index)
            status = 0
         end if
      end select
   end function write_variable_avg

   !> Reset averaging buffers for a variable
   !>
   !> This subroutine clears the averaging buffers and resets the
   !> accumulation counter, typically after writing an average.
   !>
   !> @param[inout] var_ptr  Pointer to variable to reset
   subroutine reset_avg(var_ptr)
      type(io_variable), pointer, intent(inout) :: var_ptr

      ! Safety check
      if (.not. associated(var_ptr)) return

      ! Ensure buffers exist
      call init_avg_buffers(var_ptr)

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
   end subroutine reset_avg

end module io_netcdf_avg
