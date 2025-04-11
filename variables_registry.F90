!===============================================================================
!> @file variables_registry.F90
!>
!> Variable registry for NetCDF output
!>
!> This module manages the registration of model variables with the file output
!> system. It initializes grids, applies user-configured output settings from
!> namelist, and registers variables for output.
!>
!> @author Rachid Benshila (original code)
!> @date 2025-04-11
!===============================================================================
module variables_registry
   use file_manager
   use namelist_output
   use ocean_var
   use grid_module
   use variable_definitions
   implicit none
   private

   public :: init_variables

contains
   !> Initialize all model variables for output
   !>
   !> This subroutine creates grids, initializes variable definitions,
   !> applies namelist configurations, and registers variables for output.
   !>
   !> @param[in] nx  Number of points in x-direction
   !> @param[in] ny  Number of points in y-direction
   !> @param[in] nz  Number of vertical levels
   subroutine init_variables(nx, ny, nz)
      integer, intent(in) :: nx, ny, nz
      integer :: i
      
      ! Read the output configuration from namelist
      call read_output_namelist()
      
      ! Initialize all grids
      ! 0D grid (for scalars)
      grd_scalar = create_empty_grid()
      
      ! 1D grid (for profiles)
      grd_profile = create_1d_grid(nz, "depth", "Depth", "m")
      
      ! 2D grids
      grd_rho = create_rho_grid_2d(nx, ny)
      grd_u = create_2d_grid(nx, ny, "xi_u", "eta_u", &
                             "X-dimension at u points", "Y-dimension at u points", &
                             "count", "count")
      grd_v = create_2d_grid(nx, ny, "xi_v", "eta_v", &
                             "X-dimension at v points", "Y-dimension at v points", &
                             "count", "count")
      
      ! 3D grid
      grd_rho3d = create_rho_grid_3d(nx, ny, nz)
      
      ! Initialize variable definitions
      call init_var_definitions()
      
      ! Process all variables: apply namelist config and register
      do i = 1, num_vars
         ! Apply configuration from namelist (or use defaults)
         call apply_namelist_config(vars_to_export(i))
         
         ! Register the variable for output
         call register_variable(vars_to_export(i))
      end do
      
      ! Clean up memory
      if (allocated(dyn_vars)) deallocate(dyn_vars)
   end subroutine init_variables

   !> Apply namelist configuration to a variable
   !>
   !> This subroutine searches for the variable in the namelist configuration
   !> and applies the specified settings. If not found, it applies global defaults.
   !>
   !> @param[inout] var  The variable to configure
   subroutine apply_namelist_config(var)
      type(nc_var), intent(inout) :: var
      integer :: i
      logical :: found
      
      found = .false.
      
      ! Look for this variable in the namelist configurations
      if (allocated(dyn_vars)) then
         do i = 1, size(dyn_vars)
            if (trim(dyn_vars(i)%name) == trim(var%name)) then
               ! Found - copy the configuration
               call copy_namelist_config(dyn_vars(i), var)
               found = .true.
               exit
            end if
         end do
      end if
      
      ! If not found, apply global defaults
      if (.not. found) then
         var%to_his = global_to_his
         var%to_avg = global_to_avg
         var%to_rst = global_to_rst
         var%freq_his = global_freq_his
         var%freq_avg = global_freq_avg
         var%freq_rst = global_freq_rst
         var%file_prefix = ""  ! Use global prefix
      end if
      
      ! Debug output
      print *, "Variable: ", trim(var%name)
      print *, "  His: ", var%to_his, " Freq: ", var%freq_his
      print *, "  Avg: ", var%to_avg, " Freq: ", var%freq_avg
      print *, "  Rst: ", var%to_rst, " Freq: ", var%freq_rst
      print *, "  Prefix: ", trim(var%file_prefix)
   end subroutine apply_namelist_config

   !> Copy output configuration from namelist to a variable
   !>
   !> @param[in]    dyn_var     Source configuration from namelist
   !> @param[inout] target_var  Target variable to configure
   subroutine copy_namelist_config(dyn_var, target_var)
      type(var_output_config), intent(in) :: dyn_var
      type(nc_var), intent(inout) :: target_var

      ! Copy output flags using global defaults if needed
      target_var%to_his = dyn_var%wrt
      target_var%to_avg = dyn_var%avg
      target_var%to_rst = dyn_var%rst

      ! Apply global defaults for flags if not specified (all false indicates not specified)
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
   end subroutine copy_namelist_config

end module variables_registry