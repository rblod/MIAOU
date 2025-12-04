!===============================================================================
! output_vars.inc
!
! *** SINGLE SOURCE OF TRUTH FOR OUTPUT VARIABLES ***
!
! To add a new variable for output:
!   1. Add ONE line here using the OUTVAR macro
!   2. Add the variable to the namelist file_vars list
!   That's all!
!
! Syntax:
!   OUTVAR("name", "long_name", "units", grid, ndims, fortran_variable, is_restart)
!
! Arguments:
!   name            - NetCDF variable name
!   long_name       - Descriptive name for CF convention
!   units           - Physical units
!   grid            - Grid type (grd_scalar, grd_rho, etc.)
!   ndims           - Number of spatial dimensions (0-3)
!   fortran_variable- Variable from ocean_var module
!   is_restart      - .true. if needed for restart, .false. otherwise
!
! Grids:
!   grd_scalar  - 0D scalar
!   grd_profile - 1D vertical profile
!   grd_rho     - 2D rho points
!   grd_u       - 2D u points
!   grd_v       - 2D v points
!   grd_rho3d   - 3D rho points
!
!===============================================================================

! 0D variables (scalars) - typically not in restart
OUTVAR("wind_speed", "Wind speed at 10m", "m s-1", grd_scalar, 0, wind_speed, .false.)

! 1D variables (profiles) - typically not in restart
OUTVAR("temp_profile", "Temperature profile", "degC", grd_profile, 1, temp_profile, .false.)

! 2D variables (surfaces) - prognostic variables need restart
OUTVAR("zeta", "sea surface height", "m", grd_rho, 2, zeta, .true.)
OUTVAR("u", "eastward velocity", "m s-1", grd_u, 2, u, .true.)
OUTVAR("v", "northward velocity", "m s-1", grd_v, 2, v, .true.)

! 3D variables (volumes) - prognostic variables need restart
OUTVAR("temp", "temperature", "degC", grd_rho3d, 3, temp, .true.)
