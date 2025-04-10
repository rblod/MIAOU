!===============================================================================
!> @file variables_registry.F90
!>
!> Declarations of variables with multi-dimension support
!>
!> This module manages the registration of model variables with the file output
!> system. It handles variables of different dimensions (0D to 3D) and configures
!> their output settings based on namelist information.
!>
!> @author Rachid Benshila
!> @date 2025-04-10
!===============================================================================
module variables_registry
   use file_manager
   use namelist_output
   use ocean_var
   use grid_module
   implicit none
   private

   public :: init_variables

   ! Variable declarations for different dimensions
   type(nc_var) :: var_zeta         !< 2D field (free surface)
   type(nc_var) :: var_t            !< 3D field (temperature)
   type(nc_var) :: var_u, var_v     !< 2D fields (velocity components)
   type(nc_var) :: var_wind_speed   !< 0D scalar (wind speed)
   type(nc_var) :: var_profile      !< 1D field (temperature profile)

contains
   !> Initialize all model variables for output
   !>
   !> This subroutine creates grids of various dimensions, associates data
   !> pointers with model variables, and registers them for output based on
   !> namelist configuration. It supports 0D, 1D, 2D, and 3D variables.
   !>
   !> @param[in] nx  Number of points in x-direction
   !> @param[in] ny  Number of points in y-direction
   !> @param[in] nz  Number of vertical levels
   subroutine init_variables(nx, ny, nz)
      integer, intent(in) :: nx, ny, nz
      type(grid) :: grd_0d, grd_1d, grd_2d, grd_3d
      integer :: i

      ! Read the output configuration from namelist
      call read_output_namelist()

      !-------------------------!
      ! Create grids of various dimensions
      !-------------------------!

      ! 0D grid (for scalars)
      grd_0d = create_empty_grid()

      ! 1D grid (for profiles)
      grd_1d = create_1d_grid(nz, "depth", "Depth", "m")

      ! 2D grid for zeta, u, v (existing)
      grd_2d = create_rho_grid_2d(nx, ny)

      ! 3D grid for temperature
      grd_3d = create_rho_grid_3d(nx, ny, nz)

      ! Process all variables from the namelist
      do i = 1, size(dyn_vars)
         select case (trim(dyn_vars(i)%name))

            !-------------------------!
            ! 2D variable (existing)
            !-------------------------!
         case ("zeta")
            ! Set all fields to their defaults
            var_zeta%name = "zeta"
            var_zeta%long_name = "free surface"
            var_zeta%units = "m"
            var_zeta%var_grid = grd_2d
            var_zeta%ndims = 2              ! Explicitly set dimension count

            ! Associate data pointer
            nullify (var_zeta%data_2d)
            var_zeta%data_2d => zeta

            ! Copy generic parameters from namelist
            call copy_namelist_config(dyn_vars(i), var_zeta)

            ! Register the variable
            print *
            print *, 'CALL REGISTRED_VARIABLE FOR ', var_zeta%name
            print *
            call register_variable(var_zeta)

            !-------------------------!
            ! 3D variable
            !-------------------------!
         case ("temp")
            ! Set all fields to their defaults
            var_t%name = "temp"
            var_t%long_name = "potential temperature"
            var_t%units = "degC"
            var_t%var_grid = grd_3d
            var_t%ndims = 3               ! 3D variable

            ! Associate data pointer - now we can use full 3D field
            nullify (var_t%data_3d)
            var_t%data_3d => temp

            ! Copy generic parameters from namelist
            call copy_namelist_config(dyn_vars(i), var_t)

            ! Register the variable
            print *
            print *, 'CALL REGISTRED_VARIABLE FOR ', var_t%name
            print *
            call register_variable(var_t)

            !-------------------------!
            ! 2D variable (existing)
            !-------------------------!
         case ("u")
            var_u%name = "u"
            var_u%long_name = "U-velocity"
            var_u%units = "ms-1"
            var_u%var_grid = grd_2d
            var_u%ndims = 2              ! 2D variable

            ! Associate data pointer
            nullify (var_u%data_2d)
            var_u%data_2d => u

            ! Copy generic parameters from namelist
            call copy_namelist_config(dyn_vars(i), var_u)

            ! Register the variable
            print *
            print *, 'CALL REGISTRED_VARIABLE FOR ', var_u%name
            print *
            call register_variable(var_u)

            !-------------------------!
            ! 2D variable (existing)
            !-------------------------!
         case ("v")
            var_v%name = "v"
            var_v%long_name = "V-velocity"
            var_v%units = "ms-1"
            var_v%var_grid = grd_2d
            var_v%ndims = 2              ! 2D variable

            ! Associate data pointer
            nullify (var_v%data_2d)
            var_v%data_2d => v

            ! Copy generic parameters from namelist
            call copy_namelist_config(dyn_vars(i), var_v)

            ! Register the variable
            print *
            print *, 'CALL REGISTRED_VARIABLE FOR ', var_v%name
            print *
            call register_variable(var_v)

            !-------------------------!
            ! 0D variable (new)
            !-------------------------!
         case ("wind")
            var_wind_speed%name = "wind_speed"
            var_wind_speed%long_name = "Wind speed at 10m"
            var_wind_speed%units = "ms-1"
            var_wind_speed%var_grid = grd_0d
            var_wind_speed%ndims = 0     ! 0D variable (scalar)

            ! Associate data pointer
            nullify (var_wind_speed%scalar)
            var_wind_speed%scalar => wind_speed

            ! Copy generic parameters from namelist
            call copy_namelist_config(dyn_vars(i), var_wind_speed)

            ! Register the variable
            print *
            print *, 'CALL REGISTRED_VARIABLE FOR ', var_wind_speed%name
            print *
            call register_variable(var_wind_speed)

            !-------------------------!
            ! 1D variable (new)
            !-------------------------!
         case ("profile")
            var_profile%name = "temp_profile"
            var_profile%long_name = "Temperature profile"
            var_profile%units = "degC"
            var_profile%var_grid = grd_1d
            var_profile%ndims = 1        ! 1D variable

            ! Associate data pointer
            nullify (var_profile%data_1d)
            var_profile%data_1d => temp_profile

            ! Copy generic parameters from namelist
            call copy_namelist_config(dyn_vars(i), var_profile)

            ! Register the variable
            print *
            print *, 'CALL REGISTRED_VARIABLE FOR ', var_profile%name
            print *
            call register_variable(var_profile)
         end select

      end do

      if (allocated(dyn_vars)) deallocate (dyn_vars)

   end subroutine init_variables

   !> Copy output configuration from namelist to a variable
   !>
   !> @param[in]    dyn_var     Source configuration from namelist
   !> @param[inout] target_var  Target variable to configure
   !> Copy output configuration from namelist to a variable
   !>
   !> @param[in]    dyn_var     Source configuration from namelist
   !> @param[inout] target_var  Target variable to configure
   subroutine copy_namelist_config(dyn_var, target_var)
      use namelist_output, only: global_freq_his, global_freq_avg, global_freq_rst, &
                                 global_to_his, global_to_avg, global_to_rst
      type(var_output_config), intent(in) :: dyn_var
      type(nc_var), intent(inout), target :: target_var

      ! Copy output flags using global defaults if needed
      target_var%to_his = dyn_var%wrt
      target_var%to_avg = dyn_var%avg
      target_var%to_rst = dyn_var%rst

      ! Apply global defaults for flags if not specified (value -1 indicates not specified)
      if (.not. dyn_var%wrt .and. .not. dyn_var%avg .and. .not. dyn_var%rst) then
         ! If all flags are false, use global defaults
         target_var%to_his = global_to_his
         target_var%to_avg = global_to_avg
         target_var%to_rst = global_to_rst
      end if

      ! Copy output frequencies, using global defaults if unspecified
      ! For history files
      if (dyn_var%freq_his > 0.0) then
         target_var%freq_his = dyn_var%freq_his
      else if (target_var%to_his) then
         ! Use global default if flag is enabled
         target_var%freq_his = global_freq_his
      else
         target_var%freq_his = -1.0
      end if

      ! For average files
      if (dyn_var%freq_avg > 0.0) then
         target_var%freq_avg = dyn_var%freq_avg
      else if (target_var%to_avg) then
         ! Use global default if flag is enabled
         target_var%freq_avg = global_freq_avg
      else
         target_var%freq_avg = -1.0
      end if

      ! For restart files
      if (dyn_var%freq_rst > 0.0) then
         target_var%freq_rst = dyn_var%freq_rst
      else if (target_var%to_rst) then
         ! Use global default if flag is enabled
         target_var%freq_rst = global_freq_rst
      else
         target_var%freq_rst = -1.0
      end if

      ! Copy file prefix
      target_var%file_prefix = dyn_var%file_prefix

      print *, "Variable: ", trim(target_var%name)
      print *, "  His: ", target_var%to_his, " Freq: ", target_var%freq_his
      print *, "  Avg: ", target_var%to_avg, " Freq: ", target_var%freq_avg
      print *, "  Rst: ", target_var%to_rst, " Freq: ", target_var%freq_rst
      print *, "  Prefix: ", trim(target_var%file_prefix)

   end subroutine copy_namelist_config

end module variables_registry
