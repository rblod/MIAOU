!===============================================================================
!> @file ocean_var.F90
!>
!> Enhanced module with various model variables of different dimensions
!>
!> This module contains the definition of all primary variables used in the ocean model,
!> including scalar (0D), profile (1D), surface (2D), and volume (3D) fields.
!>
!> In MPI mode, arrays are allocated with local subdomain dimensions from mpi_param.
!> In serial mode, dimensions are passed to allocate_ocean_vars.
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
   public :: allocate_ocean_vars, deallocate_ocean_vars

   !> @var zeta
   !> Free surface elevation (sea level)
   !> @dimension 2D [Lm,Mm] (local subdomain in MPI mode)
   !> @units meters
   real, allocatable :: zeta(:, :)

   !> @var u
   !> Eastward (x-direction) water velocity component
   !> @dimension 2D [Lm,Mm]
   !> @units meters per second
   real, allocatable :: u(:, :)

   !> @var v
   !> Northward (y-direction) water velocity component
   !> @dimension 2D [Lm,Mm]
   !> @units meters per second
   real, allocatable :: v(:, :)

   !> @var temp
   !> Water temperature throughout the water column
   !> @dimension 3D [Lm,Mm,N]
   !> @units degrees Celsius
   real, allocatable :: temp(:, :, :)

   !> @var wind_speed
   !> Wind speed at 10m above sea surface (scalar value)
   !> @dimension 0D [scalar]
   !> @units meters per second
   real :: wind_speed = 0.0

   !> @var temp_profile
   !> Temperature profile (typically at a single point)
   !> @dimension 1D [N]
   !> @units degrees Celsius
   real, allocatable :: temp_profile(:)

   !> @var dt
   !> Model time step
   !> @units seconds
   real :: dt = 1800.0

contains

   !---------------------------------------------------------------------------
   !> @brief Allocate ocean variables
   !>
   !> In MPI mode: uses Lm, Mm, N from mpi_param (set by mpi_setup)
   !> In serial mode: uses dimensions passed as arguments
   !>
   !> @param[in] nx_in  X dimension (ignored in MPI mode)
   !> @param[in] ny_in  Y dimension (ignored in MPI mode)
   !> @param[in] nz_in  Z dimension (ignored in MPI mode)
   !---------------------------------------------------------------------------
   subroutine allocate_ocean_vars(nx_in, ny_in, nz_in)
#ifdef MPI
      use mpi_param, only: Lm, Mm, N
#endif
      integer, intent(in), optional :: nx_in, ny_in, nz_in
      
      integer :: nx, ny, nz
      
#ifdef MPI
      ! In MPI mode, use local dimensions from mpi_param
      ! (dummy arguments preserved for interface compatibility)
      nx = Lm
      ny = Mm
      nz = N
      ! Silence unused argument warnings
      if (.false.) then
         if (present(nx_in)) nx = nx_in
         if (present(ny_in)) ny = ny_in
         if (present(nz_in)) nz = nz_in
      end if
#else
      ! In serial mode, use passed dimensions or defaults
      if (present(nx_in)) then
         nx = nx_in
      else
         nx = 50
      end if
      if (present(ny_in)) then
         ny = ny_in
      else
         ny = 40
      end if
      if (present(nz_in)) then
         nz = nz_in
      else
         nz = 10
      end if
#endif

      ! Allocate arrays
      if (.not. allocated(zeta)) allocate(zeta(nx, ny))
      if (.not. allocated(u)) allocate(u(nx, ny))
      if (.not. allocated(v)) allocate(v(nx, ny))
      if (.not. allocated(temp)) allocate(temp(nx, ny, nz))
      if (.not. allocated(temp_profile)) allocate(temp_profile(nz))

      ! Initialize to zero
      zeta = 0.0
      u = 0.0
      v = 0.0
      temp = 0.0
      temp_profile = 0.0
      wind_speed = 0.0

   end subroutine allocate_ocean_vars

   !---------------------------------------------------------------------------
   !> @brief Deallocate ocean variables
   !---------------------------------------------------------------------------
   subroutine deallocate_ocean_vars()
      
      if (allocated(zeta)) deallocate(zeta)
      if (allocated(u)) deallocate(u)
      if (allocated(v)) deallocate(v)
      if (allocated(temp)) deallocate(temp)
      if (allocated(temp_profile)) deallocate(temp_profile)

   end subroutine deallocate_ocean_vars

end module ocean_var
