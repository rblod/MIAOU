!===============================================================================
!> @file io_netcdf_avg.F90
!>
!> @brief NetCDF backend for writing averaged data
!>
!> This module provides NetCDF-specific functionality for writing time-averaged
!> variables. It uses the averaging methods encapsulated in io_variable and
!> avg_buffer.
!>
!> @see io_definitions.avg_buffer Buffer type with compute methods
!> @see netcdf_backend Low-level NetCDF write operations
!>
!> @author Rachid Benshila
!> @date 2025-04
!===============================================================================
module io_netcdf_avg
   use io_definitions, only: io_variable
   use io_averaging, only: avg_accumulate, avg_reset, avg_is_ready
   use netcdf_backend, only: nc_write_variable
   implicit none
   private

   ! Public interface
   public :: accumulate_avg
   public :: write_variable_avg
   public :: reset_avg

contains

   !---------------------------------------------------------------------------
   !> @brief Accumulate variable values for averaging
   !>
   !> Wrapper that delegates to io_averaging module.
   !>
   !> @param[inout] var_ptr  Pointer to variable to accumulate
   !---------------------------------------------------------------------------
   subroutine accumulate_avg(var_ptr)
      type(io_variable), pointer, intent(inout) :: var_ptr

      call avg_accumulate(var_ptr)
   end subroutine accumulate_avg

   !---------------------------------------------------------------------------
   !> @brief Reset averaging buffers
   !>
   !> Wrapper that delegates to io_averaging module.
   !>
   !> @param[inout] var_ptr  Pointer to variable to reset
   !---------------------------------------------------------------------------
   subroutine reset_avg(var_ptr)
      type(io_variable), pointer, intent(inout) :: var_ptr

      call avg_reset(var_ptr)
   end subroutine reset_avg

   !---------------------------------------------------------------------------
   !> @brief Write averaged variable data to a NetCDF file
   !>
   !> Computes the average using the variable's avg_buffer methods,
   !> then writes the result to the specified NetCDF file.
   !>
   !> @param[inout] var_ptr    Pointer to variable
   !> @param[in]    ncid       NetCDF file ID
   !> @param[in]    varid      Variable ID in the file
   !> @param[in]    time_index Time index to write at
   !> @return       Status code (0 = success, -1 = error)
   !---------------------------------------------------------------------------
   function write_variable_avg(var_ptr, ncid, varid, time_index) result(status)
      type(io_variable), pointer, intent(inout) :: var_ptr
      integer, intent(in) :: ncid, varid, time_index
      integer :: status

      real :: scalar_avg
      real, allocatable :: avg_1d(:), avg_2d(:,:), avg_3d(:,:,:)
      logical :: success
      integer :: shp(3)

      status = -1

      ! Safety check
      if (.not. associated(var_ptr)) return

      ! Check if we have data to write
      if (.not. avg_is_ready(var_ptr)) then
         print *, "Warning: No data accumulated for averaging: ", trim(var_ptr%meta%name)
         return
      end if

      ! Compute and write average based on dimensionality
      select case (var_ptr%meta%ndims)
      case (0)  ! Scalar
         success = var_ptr%avg%compute_scalar(scalar_avg)
         if (success) then
            call nc_write_variable(ncid, varid, scalar_avg, time_index)
            status = 0
         end if

      case (1)  ! 1D
         if (var_ptr%data%is_valid(1)) then
            call var_ptr%data%get_shape(1, shp)
            allocate(avg_1d(shp(1)))
            success = var_ptr%avg%compute_1d(avg_1d)
            if (success) then
               call nc_write_variable(ncid, varid, avg_1d, time_index)
               status = 0
            end if
            deallocate(avg_1d)
         end if

      case (2)  ! 2D
         if (var_ptr%data%is_valid(2)) then
            call var_ptr%data%get_shape(2, shp)
            allocate(avg_2d(shp(1), shp(2)))
            success = var_ptr%avg%compute_2d(avg_2d)
            if (success) then
               call nc_write_variable(ncid, varid, avg_2d, time_index)
               status = 0
            end if
            deallocate(avg_2d)
         end if

      case (3)  ! 3D
         if (var_ptr%data%is_valid(3)) then
            call var_ptr%data%get_shape(3, shp)
            allocate(avg_3d(shp(1), shp(2), shp(3)))
            success = var_ptr%avg%compute_3d(avg_3d)
            if (success) then
               call nc_write_variable(ncid, varid, avg_3d, time_index)
               status = 0
            end if
            deallocate(avg_3d)
         end if
      end select

   end function write_variable_avg

end module io_netcdf_avg
