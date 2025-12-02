!===============================================================================
!> @file io_netcdf_avg.F90
!>
!> @brief NetCDF backend for writing averaged data
!>
!> This module provides NetCDF-specific functionality for writing time-averaged
!> variables to NetCDF files. It delegates all averaging logic (accumulation,
!> buffer management, average computation) to the generic io_averaging module.
!>
!> ## Responsibilities
!>
!> - Write computed averages to NetCDF files
!> - Interface between io_averaging and NetCDF backend
!>
!> ## Usage
!>
!> ```fortran
!> ! Accumulate (delegates to io_averaging)
!> call accumulate_avg(var_ptr)
!>
!> ! Write average to NetCDF and reset
!> status = write_variable_avg(var_ptr, ncid, varid, time_index)
!> call reset_avg(var_ptr)
!> ```
!>
!> @see io_averaging Generic averaging module
!> @see netcdf_backend Low-level NetCDF operations
!>
!> @author Rachid Benshila
!> @date 2025-04
!===============================================================================
module io_netcdf_avg
   use io_definitions, only: io_variable
   use io_averaging, only: avg_init_buffers, avg_accumulate, avg_reset, &
                           avg_get_count, avg_is_ready, &
                           avg_compute_scalar, avg_compute_1d, &
                           avg_compute_2d, avg_compute_3d
   use netcdf_backend, only: nc_write_variable
   implicit none
   private

   !---------------------------------------------------------------------------
   ! Public interface
   !---------------------------------------------------------------------------
   
   !> @brief Accumulate values for averaging (delegates to io_averaging)
   public :: accumulate_avg
   
   !> @brief Write averaged data to NetCDF file
   public :: write_variable_avg
   
   !> @brief Reset averaging buffers (delegates to io_averaging)
   public :: reset_avg

contains

   !---------------------------------------------------------------------------
   !> @brief Accumulate variable values for averaging
   !>
   !> Wrapper that delegates to io_averaging module.
   !> Provided for API compatibility with existing code.
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
   !> Provided for API compatibility with existing code.
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
   !> Computes the average from accumulated data using io_averaging,
   !> then writes the result to the specified NetCDF file.
   !>
   !> @param[inout] var_ptr    Pointer to variable containing accumulated data
   !> @param[in]    ncid       NetCDF file ID
   !> @param[in]    varid      Variable ID in the NetCDF file
   !> @param[in]    time_index Time index to write at
   !> @return       Status code (0 = success, -1 = error)
   !---------------------------------------------------------------------------
   function write_variable_avg(var_ptr, ncid, varid, time_index) result(status)
      type(io_variable), pointer, intent(inout) :: var_ptr
      integer, intent(in) :: ncid, varid, time_index
      integer :: status

      ! Local arrays for computed averages
      real :: scalar_avg
      real, allocatable :: avg_1d(:), avg_2d(:,:), avg_3d(:,:,:)
      logical :: success

      status = -1

      ! Safety check
      if (.not. associated(var_ptr)) return

      ! Check if we have data to write
      if (.not. avg_is_ready(var_ptr)) then
         print *, "Warning: No data accumulated for averaging: ", trim(var_ptr%name)
         return
      end if

      ! Compute and write average based on dimensionality
      select case (var_ptr%ndims)
      case (0)  ! Scalar
         success = avg_compute_scalar(var_ptr, scalar_avg)
         if (success) then
            call nc_write_variable(ncid, varid, scalar_avg, time_index)
            status = 0
         end if

      case (1)  ! 1D
         if (associated(var_ptr%data_1d)) then
            allocate(avg_1d(size(var_ptr%data_1d)))
            success = avg_compute_1d(var_ptr, avg_1d)
            if (success) then
               call nc_write_variable(ncid, varid, avg_1d, time_index)
               status = 0
            end if
            deallocate(avg_1d)
         end if

      case (2)  ! 2D
         if (associated(var_ptr%data_2d)) then
            allocate(avg_2d(size(var_ptr%data_2d, 1), size(var_ptr%data_2d, 2)))
            success = avg_compute_2d(var_ptr, avg_2d)
            if (success) then
               call nc_write_variable(ncid, varid, avg_2d, time_index)
               status = 0
            end if
            deallocate(avg_2d)
         end if

      case (3)  ! 3D
         if (associated(var_ptr%data_3d)) then
            allocate(avg_3d(size(var_ptr%data_3d, 1), size(var_ptr%data_3d, 2), &
                           size(var_ptr%data_3d, 3)))
            success = avg_compute_3d(var_ptr, avg_3d)
            if (success) then
               call nc_write_variable(ncid, varid, avg_3d, time_index)
               status = 0
            end if
            deallocate(avg_3d)
         end if
      end select

   end function write_variable_avg

end module io_netcdf_avg
