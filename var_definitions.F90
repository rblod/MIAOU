!===============================================================================
!> @file var_definitions.F90
!>
!> Module for oceanographic variable definitions
!>
!> This module provides a declarative approach to defining output variables
!> in an oceanographic model. Users simply need to add their variables to the
!> init_var_definitions subroutine with minimal information required.
!>
!> @author Rachid Benshila
!> @date 2025-04-11
!===============================================================================
module var_definitions
   use grid_module, only: grid
   use io_definitions, only: io_variable
   use ocean_var
   implicit none
   private
   
   !> Common grid definitions (to be initialized in var_registry)
   type(grid), public :: grd_scalar  !< Grid for scalar variables
   type(grid), public :: grd_profile !< 1D grid for profiles
   type(grid), public :: grd_rho     !< 2D rho-grid (cell centers)
   type(grid), public :: grd_u       !< 2D u-grid (x-velocity points)
   type(grid), public :: grd_v       !< 2D v-grid (y-velocity points)
   type(grid), public :: grd_rho3d   !< 3D rho-grid (cell centers)
   
   !> Maximum number of variables to export
   integer, parameter :: MAX_VARS = 10
   
   !> Number of defined variables
   integer, public :: num_vars = 0
   
   !> Array of variables to be exported
   type(io_variable), public, allocatable :: model_vars(:)
   
   public :: init_var_definitions, define_0d_var, define_1d_var, define_2d_var, define_3d_var

contains
   !> Initialize all variables to be exported
   !>
   !> This is the main subroutine that users should modify when adding
   !> new variables for output. Simply add a new call to the appropriate
   !> define_Xd_var function with the required information.
   subroutine init_var_definitions()
      ! Reset the counter
      num_vars = 0
      
      ! Initialize the variables array
      if (allocated(model_vars)) deallocate(model_vars)
      allocate(model_vars(MAX_VARS))
      
      ! Define all export variables using type-specific helper functions
      call define_2d_var("zeta", "free surface", "m", grd_rho, zeta)
      call define_2d_var("u", "U-velocity", "ms-1", grd_u, u)
      call define_2d_var("v", "V-velocity", "ms-1", grd_v, v)
      call define_3d_var("temp", "potential temperature", "degC", grd_rho3d, temp)
      call define_1d_var("temp_profile", "Temperature profile", "degC", grd_profile, temp_profile)
      call define_0d_var("wind_speed", "Wind speed at 10m", "ms-1", grd_scalar, wind_speed)
      
      ! To add a new variable, simply add another call to the appropriate define_Xd_var function:
      ! call define_2d_var("new_var", "description", "units", appropriate_grid, data_pointer)
   end subroutine init_var_definitions
   
   !> Define a 0D (scalar) variable
   !>
   !> @param[in] name       Variable name for output
   !> @param[in] long_name  Long descriptive name (for attribute)
   !> @param[in] units      Units of the variable (for attribute)
   !> @param[in] var_grid   Grid associated with the variable
   !> @param[in] data       Reference to the scalar data
   subroutine define_0d_var(name, long_name, units, var_grid, data)
      character(len=*), intent(in) :: name, long_name, units
      type(grid), intent(in) :: var_grid
      real, target, intent(in) :: data
      
      ! Increment the counter
      num_vars = num_vars + 1
      
      if (num_vars > MAX_VARS) then
         print *, "ERROR: Too many variables defined. Increase MAX_VARS in var_definitions.F90"
         stop 1
      end if
      
      ! Initialize variable with basic metadata
      model_vars(num_vars)%name = name
      model_vars(num_vars)%long_name = long_name
      model_vars(num_vars)%units = units
      model_vars(num_vars)%var_grid = var_grid
      model_vars(num_vars)%ndims = 0
      
      ! Associate data pointer
      model_vars(num_vars)%scalar => data
   end subroutine define_0d_var
   
   !> Define a 1D variable
   !>
   !> @param[in] name       Variable name for output
   !> @param[in] long_name  Long descriptive name (for attribute)
   !> @param[in] units      Units of the variable (for attribute)
   !> @param[in] var_grid   Grid associated with the variable
   !> @param[in] data       Reference to the 1D array data
   subroutine define_1d_var(name, long_name, units, var_grid, data)
      character(len=*), intent(in) :: name, long_name, units
      type(grid), intent(in) :: var_grid
      real, dimension(:), target, intent(in) :: data
      
      ! Increment the counter
      num_vars = num_vars + 1
      
      if (num_vars > MAX_VARS) then
         print *, "ERROR: Too many variables defined. Increase MAX_VARS in var_definitions.F90"
         stop 1
      end if
      
      ! Initialize variable with basic metadata
      model_vars(num_vars)%name = name
      model_vars(num_vars)%long_name = long_name
      model_vars(num_vars)%units = units
      model_vars(num_vars)%var_grid = var_grid
      model_vars(num_vars)%ndims = 1
      
      ! Associate data pointer
      model_vars(num_vars)%data_1d => data
   end subroutine define_1d_var
   
   !> Define a 2D variable
   !>
   !> @param[in] name       Variable name for output
   !> @param[in] long_name  Long descriptive name (for attribute)
   !> @param[in] units      Units of the variable (for attribute)
   !> @param[in] var_grid   Grid associated with the variable
   !> @param[in] data       Reference to the 2D array data
   subroutine define_2d_var(name, long_name, units, var_grid, data)
      character(len=*), intent(in) :: name, long_name, units
      type(grid), intent(in) :: var_grid
      real, dimension(:,:), target, intent(in) :: data
      
      ! Increment the counter
      num_vars = num_vars + 1
      
      if (num_vars > MAX_VARS) then
         print *, "ERROR: Too many variables defined. Increase MAX_VARS in var_definitions.F90"
         stop 1
      end if
      
      ! Initialize variable with basic metadata
      model_vars(num_vars)%name = name
      model_vars(num_vars)%long_name = long_name
      model_vars(num_vars)%units = units
      model_vars(num_vars)%var_grid = var_grid
      model_vars(num_vars)%ndims = 2
      
      ! Associate data pointer
      model_vars(num_vars)%data_2d => data
   end subroutine define_2d_var
   
   !> Define a 3D variable
   !>
   !> @param[in] name       Variable name for output
   !> @param[in] long_name  Long descriptive name (for attribute)
   !> @param[in] units      Units of the variable (for attribute)
   !> @param[in] var_grid   Grid associated with the variable
   !> @param[in] data       Reference to the 3D array data
   subroutine define_3d_var(name, long_name, units, var_grid, data)
      character(len=*), intent(in) :: name, long_name, units
      type(grid), intent(in) :: var_grid
      real, dimension(:,:,:), target, intent(in) :: data
      
      ! Increment the counter
      num_vars = num_vars + 1
      
      if (num_vars > MAX_VARS) then
         print *, "ERROR: Too many variables defined. Increase MAX_VARS in var_definitions.F90"
         stop 1
      end if
      
      ! Initialize variable with basic metadata
      model_vars(num_vars)%name = name
      model_vars(num_vars)%long_name = long_name
      model_vars(num_vars)%units = units
      model_vars(num_vars)%var_grid = var_grid
      model_vars(num_vars)%ndims = 3
      
      ! Associate data pointer
      model_vars(num_vars)%data_3d => data
   end subroutine define_3d_var
   
end module var_definitions