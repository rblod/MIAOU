!===============================================================================
!> @file ocean_var.F90
!>
!> Enhanced module with various model variables of different dimensions
!>
!> This module contains the definition of all primary variables used in the ocean model,
!> including scalar (0D), profile (1D), surface (2D), and volume (3D) fields.
!>
!> NOTE: Variables do NOT need 'target' attribute. MIAOU uses internal buffers.
!>
!> @author Rachid Benshila
!> @date 2025-04-10
!===============================================================================
module ocean_var
   implicit none
   private
   public :: zeta, u, v, temp, wind_speed, temp_profile, dt

   !> @var zeta
   !> Free surface elevation (sea level)
   !> @dimension 2D [nx,ny]
   !> @units meters
   real, allocatable :: zeta(:, :)

   !> @var u
   !> Eastward (x-direction) water velocity component
   !> @dimension 2D [nx,ny]
   !> @units meters per second
   real, allocatable :: u(:, :)

   !> @var v
   !> Northward (y-direction) water velocity component
   !> @dimension 2D [nx,ny]
   !> @units meters per second
   real, allocatable :: v(:, :)

   !> @var temp
   !> Water temperature throughout the water column
   !> @dimension 3D [nx,ny,nz]
   !> @units degrees Celsius
   real, allocatable :: temp(:, :, :)

   !> @var wind_speed
   !> Wind speed at 10m above sea surface (scalar value)
   !> @dimension 0D [scalar]
   !> @units meters per second
   real :: wind_speed = 0.0

   !> @var temp_profile
   !> Temperature profile (typically at a single point)
   !> @dimension 1D [nz]
   !> @units degrees Celsius
   real, allocatable :: temp_profile(:)

   !> @var dt
   !> Model time step
   !> @units seconds
   real :: dt = 1800.0

end module ocean_var
