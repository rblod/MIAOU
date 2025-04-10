!===============================================================================
!> variables_registry_enhanced.F90 : Declarations of variables with multi-dimension support
!===============================================================================
module variables_registry
   use file_manager
   use namelist_output
   use ocean_var
   use grid_module
   implicit none

   ! Variable declarations for different dimensions
   type(nc_var) :: var_zeta         ! 2D field (existing)
   type(nc_var) :: var_t            ! 3D field
   type(nc_var) :: var_u, var_v      ! 2D fields (existing)
   type(nc_var) :: var_wind_speed    ! 0D scalar
   type(nc_var) :: var_profile       ! 1D field

contains
   !>
   !> Initializes all nc_var variables with associated metadata and grids.
   !> Now supports 0D, 1D, 2D, and 3D variables
   !>
   !> @param nx, ny, nz
   !>
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
            nullify(var_zeta%data_2d)
            var_zeta%data_2d => zeta

            ! Copie générique des paramètres
            call copy_namelist_config(dyn_vars(i), var_zeta)
            
            ! Générer automatiquement les noms de fichiers
            !call set_variable_filenames(var_zeta)
            
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
            nullify(var_t%data_3d)
            var_t%data_3d => temp

            ! Copie générique des paramètres
            call copy_namelist_config(dyn_vars(i), var_t)
            
            ! Générer automatiquement les noms de fichiers
            !call set_variable_filenames(var_t)
            
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
            nullify(var_u%data_2d)
            var_u%data_2d => u


            ! Copie générique des paramètres
            call copy_namelist_config(dyn_vars(i), var_u)
            
            ! Générer automatiquement les noms de fichiers
            !call set_variable_filenames(var_u)
            
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
            nullify(var_v%data_2d)
            var_v%data_2d => v

            ! Copie générique des paramètres
            call copy_namelist_config(dyn_vars(i), var_v)
         
            ! Générer automatiquement les noms de fichiers
            !call set_variable_filenames(var_v)
            
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
            nullify(var_wind_speed%scalar)
            var_wind_speed%scalar => wind_speed

            ! Copie générique des paramètres
            call copy_namelist_config(dyn_vars(i), var_wind_speed)
         
            ! Générer automatiquement les noms de fichiers
            !call set_variable_filenames(var_wind_speed)
            
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
            nullify(var_profile%data_1d)
            var_profile%data_1d => temp_profile

            ! Copie générique des paramètres
            call copy_namelist_config(dyn_vars(i), var_profile)
         
            ! Générer automatiquement les noms de fichiers
            !call set_variable_filenames(var_profile)
            
            ! Register the variable
            print *
            print *, 'CALL REGISTRED_VARIABLE FOR ', var_profile%name
            print *
            call register_variable(var_profile)
         end select

      end do

      if (allocated(dyn_vars)) deallocate(dyn_vars)

   end subroutine init_variables

   ! subroutine set_variable_filenames(var)
   !    type(nc_var), intent(inout) :: var
      
   !    ! Générer les filenames basés sur les fréquences
   !    if (var%to_his .and. var%freq_his > 0) then
   !       var%filename_his = generate_filename(var%file_prefix, "his", var%freq_his)
   !    end if
      
   !    if (var%to_avg .and. var%freq_avg > 0) then
   !       var%filename_avg = generate_filename(var%file_prefix, "avg", var%freq_avg)
   !    end if
      
   !    if (var%to_rst .and. var%freq_rst > 0) then
   !       var%filename_rst = generate_filename(var%file_prefix, "rst", var%freq_rst)
   !    end if
   ! end subroutine set_variable_filenames 
   
   subroutine copy_namelist_config(dyn_var, target_var)
      type(var_output_config), intent(in) :: dyn_var
      type(nc_var), intent(inout), target :: target_var

      ! Copier les paramètres de sortie
      target_var%to_his = dyn_var%wrt
      target_var%to_avg = dyn_var%avg
      target_var%to_rst = dyn_var%rst
      
      ! Copier les fréquences
      target_var%freq_his = dyn_var%freq_his
      target_var%freq_avg = dyn_var%freq_avg
      target_var%freq_rst = dyn_var%freq_rst
      
      ! Copier le préfixe de fichier
      target_var%file_prefix = dyn_var%file_prefix
   end subroutine copy_namelist_config

end module variables_registry