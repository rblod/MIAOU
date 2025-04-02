!===============================================================================
!> Variables_registry.F90 : Declarations of all nc_var variables with defaults
!===============================================================================
!>
!>
!>
!>
module variables_registry
   use file_manager
   use namelist_output
   use ocean_var
   implicit none

   type(nc_var) :: var_zeta, var_t, var_u, var_v

contains
!>
!>
!> Initializes all nc_var variables with associated metadata and grids.
!>
!> @param nx, ny, nz
!>
!> @param nx, ny, nz

   subroutine init_variables(nx, ny, nz)
      integer, intent(in) :: nx, ny, nz
      type(grid) :: grd_2d, grd_3d
      integer :: i

      call read_output_namelist()

      !-------------------------!
      ! 2D grid for zeta
      !-------------------------!
      grd_2d%name = "rho2d"
      allocate (grd_2d%dim_names(2))
      allocate (grd_2d%dim_sizes(2))
      allocate (grd_2d%dim_ids(2))
      grd_2d%dim_names = [character(len=7) :: "xi_rho", "eta_rho"]
      grd_2d%dim_sizes = [nx, ny]
      grd_2d%dim_ids = -1

      !-------------------------!
      ! 3D grid for temperature
      !-------------------------!
      grd_3d%name = "rho3d"
      allocate (grd_3d%dim_names(3))
      allocate (grd_3d%dim_sizes(3))
      allocate (grd_3d%dim_ids(3))
      grd_3d%dim_names = [character(len=7) :: "xi_rho", "eta_rho", "s_rho"]
      grd_3d%dim_sizes = [nx, ny, nz]
      grd_3d%dim_ids = -1

      ! Parcours des dyn_vars lus dans la namelist
      do i = 1, size(dyn_vars)
         select case (trim(dyn_vars(i)%name))
         case ("zeta")
            nullify (var_zeta%data)
            var_zeta%name = "zeta"
            var_zeta%long_name = "free surface"
            var_zeta%units = "m"
            var_zeta%var_grid = grd_2d
            var_zeta%to_his = dyn_vars(i)%wrt
            var_zeta%to_avg = dyn_vars(i)%avg
            var_zeta%to_rst = dyn_vars(i)%rst
            var_zeta%freq_his = dyn_vars(i)%freq_his
            var_zeta%freq_avg = dyn_vars(i)%freq_avg
            var_zeta%freq_rst = dyn_vars(i)%freq_rst
            var_zeta%file_prefix = dyn_vars(i)%file_prefix  ! Stocke le préfixe de fichier
            var_zeta%data => zeta
            call register_variable(var_zeta)
         case ("temp")
            nullify (var_t%data)
            var_t%name = "temp"
            var_t%long_name = "potential temperature"
            var_t%units = "degC"
            var_t%var_grid = grd_3d
            var_t%to_his = dyn_vars(i)%wrt
            var_t%to_avg = dyn_vars(i)%avg
            var_t%to_rst = dyn_vars(i)%rst
            var_t%freq_his = dyn_vars(i)%freq_his
            var_t%freq_avg = dyn_vars(i)%freq_avg
            var_t%freq_rst = dyn_vars(i)%freq_rst
            var_t%file_prefix = dyn_vars(i)%file_prefix  ! Stocke le préfixe de fichier
            var_t%data => temp(:, :, 1)
            call register_variable(var_t)
         case ("u")
            nullify (var_u%data)
            var_u%name = "u"
            var_u%long_name = "U-velocity"
            var_u%units = "ms-1"
            var_u%var_grid = grd_2d
            var_u%to_his = dyn_vars(i)%wrt
            var_u%to_avg = dyn_vars(i)%avg
            var_u%to_rst = dyn_vars(i)%rst
            var_u%freq_his = dyn_vars(i)%freq_his
            var_u%freq_avg = dyn_vars(i)%freq_avg
            var_u%freq_rst = dyn_vars(i)%freq_rst
            var_u%file_prefix = dyn_vars(i)%file_prefix  ! Stocke le préfixe de fichier
            var_u%data => u
            call register_variable(var_u)
         case ("v")
            nullify (var_v%data)
            var_v%name = "v"
            var_v%long_name = "V-velocity"
            var_v%units = "ms-1"
            var_v%var_grid = grd_2d
            var_v%to_his = dyn_vars(i)%wrt
            var_v%to_avg = dyn_vars(i)%avg
            var_v%to_rst = dyn_vars(i)%rst
            var_v%freq_his = dyn_vars(i)%freq_his
            var_v%freq_avg = dyn_vars(i)%freq_avg
            var_v%freq_rst = dyn_vars(i)%freq_rst
            var_v%file_prefix = dyn_vars(i)%file_prefix  ! Stocke le préfixe de fichier
            var_v%data => v(:, :)
            call register_variable(var_v)
         end select
      end do

   end subroutine init_variables

end module variables_registry
