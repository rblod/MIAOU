!===============================================================================
!> @file io_netcdf_avg.F90
!>
!> NetCDF averaging implementation
!>
!> This module provides NetCDF-specific implementations for time averaging
!> operations used in the output system.
!>
!> @author Rachid Benshila
!> @date 2025-04-11
!===============================================================================
module io_netcdf_avg
   use io_definitions, only: io_variable
   use netcdf_backend, only: nc_define_variable, nc_write_variable

   use ocean_var, only: dt
   implicit none
   private

   ! Public procedures
   public :: init_avg_buffers, accumulate_avg, write_variable_avg, reset_avg

   ! Constants
   real, parameter :: TOLERANCE = 1.0e-5

contains

   subroutine init_avg_buffers(var)
      type(io_variable), intent(inout) :: var

      ! Retourner immédiatement si déjà initialisé
      if (var%avg_initialized) return

      select case (var%ndims)
      case (0)  ! Scalaire
         var%scalar_avg = 0.0
      case (1)  ! 1D
         if (associated(var%data_1d)) then
            if (allocated(var%data_avg_1d)) deallocate (var%data_avg_1d)
            allocate (var%data_avg_1d(size(var%data_1d)))
            var%data_avg_1d = 0.0
         end if
      case (2)  ! 2D
         if (associated(var%data_2d)) then
            if (allocated(var%data_avg_2d)) deallocate (var%data_avg_2d)
            allocate (var%data_avg_2d(size(var%data_2d, 1), size(var%data_2d, 2)))
            var%data_avg_2d = 0.0
         end if
      case (3)  ! 3D
         if (associated(var%data_3d)) then
            if (allocated(var%data_avg_3d)) deallocate (var%data_avg_3d)
            allocate (var%data_avg_3d(size(var%data_3d, 1), size(var%data_3d, 2), size(var%data_3d, 3)))
            var%data_avg_3d = 0.0
         end if
      end select

      var%avg_count = 0
      var%avg_initialized = .true.
   end subroutine init_avg_buffers

   subroutine accumulate_avg(var)
      type(io_variable), intent(inout) :: var

      if (.not. var%to_avg) return  ! Ne pas accumuler si non requis

      ! Initialiser les buffers si nécessaire
      call init_avg_buffers(var)

      ! Accumuler selon les dimensions
      select case (var%ndims)
      case (0)  ! Scalaire
         if (associated(var%scalar)) then
            var%scalar_avg = var%scalar_avg + var%scalar
         end if
      case (1)  ! 1D
         if (associated(var%data_1d) .and. allocated(var%data_avg_1d)) then
            var%data_avg_1d = var%data_avg_1d + var%data_1d
         end if
      case (2)  ! 2D
         if (associated(var%data_2d) .and. allocated(var%data_avg_2d)) then
            var%data_avg_2d = var%data_avg_2d + var%data_2d
         end if
      case (3)  ! 3D
         if (associated(var%data_3d) .and. allocated(var%data_avg_3d)) then
            var%data_avg_3d = var%data_avg_3d + var%data_3d
         end if
      end select

      var%avg_count = var%avg_count + 1
   end subroutine accumulate_avg

   function write_variable_avg(var, ncid, varid, time_index) result(status)
      type(io_variable), intent(inout) :: var
      integer, intent(in) :: ncid, varid, time_index
      integer :: status

      status = -1

      call init_avg_buffers(var)
      if (var%avg_count == 0) return  ! Rien à écrire si pas d'accumulation

      ! Calculer la moyenne et écrire
      select case (var%ndims)
      case (0)  ! Scalaire
         call nc_write_variable(ncid, varid, var%scalar_avg/var%avg_count, time_index)
      case (1)  ! 1D
         if (allocated(var%data_avg_1d)) then
            call nc_write_variable(ncid, varid, var%data_avg_1d/var%avg_count, time_index)
         end if
      case (2)  ! 2D
         if (allocated(var%data_avg_2d)) then
            call nc_write_variable(ncid, varid, var%data_avg_2d/var%avg_count, time_index)
         end if
      case (3)  ! 3D
         if (allocated(var%data_avg_3d)) then
            call nc_write_variable(ncid, varid, var%data_avg_3d/var%avg_count, time_index)
         end if
      end select

      status = 0
   end function write_variable_avg

   subroutine reset_avg(var)
      type(io_variable), intent(inout) :: var

      call init_avg_buffers(var)

      select case (var%ndims)
      case (0)  ! Scalaire
         var%scalar_avg = 0.0
      case (1)  ! 1D
         if (allocated(var%data_avg_1d)) var%data_avg_1d = 0.0
      case (2)  ! 2D
         if (allocated(var%data_avg_2d)) var%data_avg_2d = 0.0
      case (3)  ! 3D
         if (allocated(var%data_avg_3d)) var%data_avg_3d = 0.0
      end select

      var%avg_count = 0
   end subroutine reset_avg
end module io_netcdf_avg
