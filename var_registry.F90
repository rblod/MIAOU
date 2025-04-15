!===============================================================================
!> @file var_registry.F90
!>
!> Variable registry for output system
!>
!> This module manages the registration of model variables with the output
!> system. It initializes grids, applies user-configured output settings from
!> configuration files, and registers variables for output.
!>
!> @author Rachid Benshila
!> @date 2025-04-11
!===============================================================================
module var_registry
   use grid_module
   use io_definitions, only: io_variable
   use io_manager, only: register_variable
   use var_definitions
   use ocean_var, only: dt
   implicit none
   private

   public :: init_variables

contains
   !> Initialize all model variables for output
   !>
   !> This subroutine creates grids, initializes variable definitions,
   !> and registers variables for output.
   !>
   !> @param[in] nx  Number of points in x-direction
   !> @param[in] ny  Number of points in y-direction
   !> @param[in] nz  Number of vertical levels
   subroutine init_variables(nx, ny, nz)
      integer, intent(in) :: nx, ny, nz
      integer :: i
      type(axis) :: dummy_axis

      ! Initialize all grids
      ! 0D grid (for scalars)
      grd_scalar = create_empty_grid()
      grd_scalar%name = "scalar"
      
      ! Special handling for scalars - add a dummy axis
      dummy_axis = create_axis("dummy", "Dummy dimension", "count", 1)
      call grd_scalar%add_axis(dummy_axis)
      
      ! 1D grid (for profiles)
      grd_profile = create_1d_grid(nz, "depth", "Depth", "m")
      grd_profile%name = "profile"
      
      ! 2D grids
      grd_rho = create_rho_grid_2d(nx, ny)
      grd_u = create_2d_grid(nx, ny, "xi_u", "eta_u", &
                             "X-dimension at u points", "Y-dimension at u points", &
                             "count", "count")
      grd_u%name = "u_grid"
      
      grd_v = create_2d_grid(nx, ny, "xi_v", "eta_v", &
                             "X-dimension at v points", "Y-dimension at v points", &
                             "count", "count")
      grd_v%name = "v_grid"
      
      ! 3D grid
      grd_rho3d = create_rho_grid_3d(nx, ny, nz)
      
      ! Print grid diagnostics
      print *, "=== Grid Diagnostics ==="
      print *, "  scalar:", grd_scalar%ndims, "name:", trim(grd_scalar%name)
      if (allocated(grd_scalar%axes)) then
         print *, "    axis 1:", trim(grd_scalar%axes(1)%name), "size:", grd_scalar%axes(1)%size
      end if
      
      print *, "  profile:", grd_profile%ndims, "name:", trim(grd_profile%name)
      if (allocated(grd_profile%axes)) then
         print *, "    axis 1:", trim(grd_profile%axes(1)%name), "size:", grd_profile%axes(1)%size
      end if
      
      print *, "  rho:", grd_rho%ndims, "name:", trim(grd_rho%name)
      if (allocated(grd_rho%axes)) then
         print *, "    axis 1:", trim(grd_rho%axes(1)%name), "size:", grd_rho%axes(1)%size
         print *, "    axis 2:", trim(grd_rho%axes(2)%name), "size:", grd_rho%axes(2)%size
      end if
      
      print *, "  rho3d:", grd_rho3d%ndims, "name:", trim(grd_rho3d%name)
      if (allocated(grd_rho3d%axes)) then
         print *, "    axis 1:", trim(grd_rho3d%axes(1)%name), "size:", grd_rho3d%axes(1)%size
         print *, "    axis 2:", trim(grd_rho3d%axes(2)%name), "size:", grd_rho3d%axes(2)%size
         print *, "    axis 3:", trim(grd_rho3d%axes(3)%name), "size:", grd_rho3d%axes(3)%size
      end if
      print *, "========================"
      
      ! Initialize variable definitions
      call init_var_definitions()
      
      ! Register all variables with the I/O system
      do i = 1, num_vars
         ! Print debugging information
         print *, "Registering variable:", trim(model_vars(i)%name)
         print *, "  Grid name:", trim(model_vars(i)%var_grid%name)
         print *, "  Dimensions:", model_vars(i)%ndims
         
         ! Register the variable for output
         call register_variable(model_vars(i))
      end do
   end subroutine init_variables
   
end module var_registry