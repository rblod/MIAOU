!===============================================================================
!> @file var_definitions.F90
!>
!> @brief Module for oceanographic variable definitions
!>
!> This module provides a declarative approach to defining output variables
!> in an oceanographic model. Users simply need to add their variables to the
!> init_var_definitions subroutine with minimal information required.
!>
!> @author Rachid Benshila
!> @date 2025-04
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

   !---------------------------------------------------------------------------
   !> @brief Initialize all variables to be exported
   !>
   !> This is the main subroutine that users should modify when adding
   !> new variables for output. Simply add a new call to the appropriate
   !> define_Xd_var function with the required information.
   !>
   !> CF-compliant attributes (optional):
   !> - standard_name: CF standard name from CF convention table
   !> - valid_min/valid_max: Valid data range for quality control
   !> - coordinates: Coordinate variables (e.g., "lat lon")
   !---------------------------------------------------------------------------
   subroutine init_var_definitions()
      ! Reset the counter
      num_vars = 0

      ! Initialize the variables array
      if (allocated(model_vars)) deallocate(model_vars)
      allocate(model_vars(MAX_VARS))

      ! Define all export variables using type-specific helper functions
      ! Example with CF-compliant attributes:
      call define_2d_var("zeta", "free surface", "m", grd_rho, zeta, &
                         standard_name="sea_surface_height_above_geoid", &
                         valid_min=-10.0, valid_max=10.0, &
                         coordinates="lat lon")
      
      call define_2d_var("u", "U-velocity", "m s-1", grd_u, u, &
                         standard_name="sea_water_x_velocity", &
                         valid_min=-10.0, valid_max=10.0)
      
      call define_2d_var("v", "V-velocity", "m s-1", grd_v, v, &
                         standard_name="sea_water_y_velocity", &
                         valid_min=-10.0, valid_max=10.0)
      
      call define_3d_var("temp", "potential temperature", "degC", grd_rho3d, temp, &
                         standard_name="sea_water_potential_temperature", &
                         valid_min=-2.0, valid_max=40.0)
      
      call define_1d_var("temp_profile", "Temperature profile", "degC", grd_profile, temp_profile)
      
      call define_0d_var("wind_speed", "Wind speed at 10m", "m s-1", grd_scalar, wind_speed, &
                         standard_name="wind_speed", &
                         valid_min=0.0, valid_max=100.0)

      ! To add a new variable, simply add another call to the appropriate define_Xd_var function:
      ! call define_2d_var("new_var", "description", "units", appropriate_grid, data_pointer, &
      !                    standard_name="cf_standard_name", valid_min=0.0, valid_max=100.0)
   end subroutine init_var_definitions

   !---------------------------------------------------------------------------
   !> @brief Define a 0D (scalar) variable
   !>
   !> @param[in] name          Variable name for output
   !> @param[in] long_name     Long descriptive name (for attribute)
   !> @param[in] units         Units of the variable (for attribute)
   !> @param[in] var_grid      Grid associated with the variable
   !> @param[in] data          Reference to the scalar data
   !> @param[in] standard_name Optional: CF standard name
   !> @param[in] valid_min     Optional: Minimum valid value
   !> @param[in] valid_max     Optional: Maximum valid value
   !> @param[in] coordinates   Optional: Coordinate variables
   !---------------------------------------------------------------------------
   subroutine define_0d_var(name, long_name, units, var_grid, data, &
                            standard_name, valid_min, valid_max, coordinates)
      character(len=*), intent(in) :: name, long_name, units
      type(grid), intent(in) :: var_grid
      real, target, intent(in) :: data
      character(len=*), intent(in), optional :: standard_name
      real, intent(in), optional :: valid_min, valid_max
      character(len=*), intent(in), optional :: coordinates

      ! Increment the counter
      num_vars = num_vars + 1

      if (num_vars > MAX_VARS) then
         print *, "ERROR: Too many variables defined. Increase MAX_VARS in var_definitions.F90"
         stop 1
      end if

      ! Initialize metadata
      model_vars(num_vars)%meta%name = name
      model_vars(num_vars)%meta%long_name = long_name
      model_vars(num_vars)%meta%units = units
      model_vars(num_vars)%meta%var_grid = var_grid
      model_vars(num_vars)%meta%ndims = 0

      ! CF-compliant attributes
      if (present(standard_name)) model_vars(num_vars)%meta%standard_name = standard_name
      if (present(valid_min)) model_vars(num_vars)%meta%valid_min = valid_min
      if (present(valid_max)) model_vars(num_vars)%meta%valid_max = valid_max
      if (present(coordinates)) model_vars(num_vars)%meta%coordinates = coordinates

      ! Associate data pointer
      model_vars(num_vars)%data%scalar => data
   end subroutine define_0d_var

   !---------------------------------------------------------------------------
   !> @brief Define a 1D variable
   !>
   !> @param[in] name          Variable name for output
   !> @param[in] long_name     Long descriptive name (for attribute)
   !> @param[in] units         Units of the variable (for attribute)
   !> @param[in] var_grid      Grid associated with the variable
   !> @param[in] data          Reference to the 1D array data
   !> @param[in] standard_name Optional: CF standard name
   !> @param[in] valid_min     Optional: Minimum valid value
   !> @param[in] valid_max     Optional: Maximum valid value
   !> @param[in] coordinates   Optional: Coordinate variables
   !---------------------------------------------------------------------------
   subroutine define_1d_var(name, long_name, units, var_grid, data, &
                            standard_name, valid_min, valid_max, coordinates)
      character(len=*), intent(in) :: name, long_name, units
      type(grid), intent(in) :: var_grid
      real, dimension(:), target, intent(in) :: data
      character(len=*), intent(in), optional :: standard_name
      real, intent(in), optional :: valid_min, valid_max
      character(len=*), intent(in), optional :: coordinates

      ! Increment the counter
      num_vars = num_vars + 1

      if (num_vars > MAX_VARS) then
         print *, "ERROR: Too many variables defined. Increase MAX_VARS in var_definitions.F90"
         stop 1
      end if

      ! Initialize metadata
      model_vars(num_vars)%meta%name = name
      model_vars(num_vars)%meta%long_name = long_name
      model_vars(num_vars)%meta%units = units
      model_vars(num_vars)%meta%var_grid = var_grid
      model_vars(num_vars)%meta%ndims = 1

      ! CF-compliant attributes
      if (present(standard_name)) model_vars(num_vars)%meta%standard_name = standard_name
      if (present(valid_min)) model_vars(num_vars)%meta%valid_min = valid_min
      if (present(valid_max)) model_vars(num_vars)%meta%valid_max = valid_max
      if (present(coordinates)) model_vars(num_vars)%meta%coordinates = coordinates

      ! Associate data pointer
      model_vars(num_vars)%data%d1 => data
   end subroutine define_1d_var

   !---------------------------------------------------------------------------
   !> @brief Define a 2D variable
   !>
   !> @param[in] name          Variable name for output
   !> @param[in] long_name     Long descriptive name (for attribute)
   !> @param[in] units         Units of the variable (for attribute)
   !> @param[in] var_grid      Grid associated with the variable
   !> @param[in] data          Reference to the 2D array data
   !> @param[in] standard_name Optional: CF standard name
   !> @param[in] valid_min     Optional: Minimum valid value
   !> @param[in] valid_max     Optional: Maximum valid value
   !> @param[in] coordinates   Optional: Coordinate variables
   !---------------------------------------------------------------------------
   subroutine define_2d_var(name, long_name, units, var_grid, data, &
                            standard_name, valid_min, valid_max, coordinates)
      character(len=*), intent(in) :: name, long_name, units
      type(grid), intent(in) :: var_grid
      real, dimension(:,:), target, intent(in) :: data
      character(len=*), intent(in), optional :: standard_name
      real, intent(in), optional :: valid_min, valid_max
      character(len=*), intent(in), optional :: coordinates

      ! Increment the counter
      num_vars = num_vars + 1

      if (num_vars > MAX_VARS) then
         print *, "ERROR: Too many variables defined. Increase MAX_VARS in var_definitions.F90"
         stop 1
      end if

      ! Initialize metadata
      model_vars(num_vars)%meta%name = name
      model_vars(num_vars)%meta%long_name = long_name
      model_vars(num_vars)%meta%units = units
      model_vars(num_vars)%meta%var_grid = var_grid
      model_vars(num_vars)%meta%ndims = 2

      ! CF-compliant attributes
      if (present(standard_name)) model_vars(num_vars)%meta%standard_name = standard_name
      if (present(valid_min)) model_vars(num_vars)%meta%valid_min = valid_min
      if (present(valid_max)) model_vars(num_vars)%meta%valid_max = valid_max
      if (present(coordinates)) model_vars(num_vars)%meta%coordinates = coordinates

      ! Associate data pointer
      model_vars(num_vars)%data%d2 => data
   end subroutine define_2d_var

   !---------------------------------------------------------------------------
   !> @brief Define a 3D variable
   !>
   !> @param[in] name          Variable name for output
   !> @param[in] long_name     Long descriptive name (for attribute)
   !> @param[in] units         Units of the variable (for attribute)
   !> @param[in] var_grid      Grid associated with the variable
   !> @param[in] data          Reference to the 3D array data
   !> @param[in] standard_name Optional: CF standard name
   !> @param[in] valid_min     Optional: Minimum valid value
   !> @param[in] valid_max     Optional: Maximum valid value
   !> @param[in] coordinates   Optional: Coordinate variables
   !---------------------------------------------------------------------------
   subroutine define_3d_var(name, long_name, units, var_grid, data, &
                            standard_name, valid_min, valid_max, coordinates)
      character(len=*), intent(in) :: name, long_name, units
      type(grid), intent(in) :: var_grid
      real, dimension(:,:,:), target, intent(in) :: data
      character(len=*), intent(in), optional :: standard_name
      real, intent(in), optional :: valid_min, valid_max
      character(len=*), intent(in), optional :: coordinates

      ! Increment the counter
      num_vars = num_vars + 1

      if (num_vars > MAX_VARS) then
         print *, "ERROR: Too many variables defined. Increase MAX_VARS in var_definitions.F90"
         stop 1
      end if

      ! Initialize metadata
      model_vars(num_vars)%meta%name = name
      model_vars(num_vars)%meta%long_name = long_name
      model_vars(num_vars)%meta%units = units
      model_vars(num_vars)%meta%var_grid = var_grid
      model_vars(num_vars)%meta%ndims = 3

      ! CF-compliant attributes
      if (present(standard_name)) model_vars(num_vars)%meta%standard_name = standard_name
      if (present(valid_min)) model_vars(num_vars)%meta%valid_min = valid_min
      if (present(valid_max)) model_vars(num_vars)%meta%valid_max = valid_max
      if (present(coordinates)) model_vars(num_vars)%meta%coordinates = coordinates

      ! Associate data pointer
      model_vars(num_vars)%data%d3 => data
   end subroutine define_3d_var

end module var_definitions
