!===============================================================================
!> @file var_definitions.F90
!>
!> @brief Module for oceanographic variable definitions
!>
!> Variables are defined in a SINGLE file: output_vars.inc
!> To add a new variable, edit ONLY that file.
!>
!> Model variables do NOT need the 'target' attribute - MIAOU uses internal buffers.
!>
!> @author Rachid Benshila
!> @date 2025-04
!===============================================================================
module var_definitions
   use grid_module, only: grid
   use io_definitions, only: io_variable
   use io_manager, only: var_registry, get_variable_ptr, process_var_instant, &
                         process_var_average
   use ocean_var
   implicit none
   private

   !> Grids
   type(grid), public :: grd_scalar
   type(grid), public :: grd_profile
   type(grid), public :: grd_rho
   type(grid), public :: grd_u
   type(grid), public :: grd_v
   type(grid), public :: grd_rho3d

   integer, parameter :: MAX_VARS = 100
   integer, public :: num_vars = 0
   type(io_variable), public, allocatable, target :: model_vars(:)

   !> Restart flags for each variable (indexed by name)
   character(len=64), private :: restart_var_names(MAX_VARS)
   integer, private :: num_restart_vars = 0

   public :: init_var_definitions
   public :: send_all_outputs
   public :: send_var

   !> Generic interface for send_var
   interface send_var
      module procedure send_var_0d
      module procedure send_var_1d
      module procedure send_var_2d
      module procedure send_var_3d
   end interface send_var

contains

   !---------------------------------------------------------------------------
   !> @brief Check if a variable needs restart buffer (local, fast lookup)
   !---------------------------------------------------------------------------
   function needs_restart_buffer(var_name) result(needs)
      character(len=*), intent(in) :: var_name
      logical :: needs
      integer :: i

      needs = .false.
      do i = 1, num_restart_vars
         if (trim(restart_var_names(i)) == trim(var_name)) then
            needs = .true.
            return
         end if
      end do
   end function needs_restart_buffer

   !---------------------------------------------------------------------------
   !> @brief Register a variable as needing restart buffer
   !---------------------------------------------------------------------------
   subroutine register_restart_var(var_name)
      character(len=*), intent(in) :: var_name

      num_restart_vars = num_restart_vars + 1
      restart_var_names(num_restart_vars) = var_name
   end subroutine register_restart_var

   !---------------------------------------------------------------------------
   !> @brief Initialize all variables to be exported
   !---------------------------------------------------------------------------
   subroutine init_var_definitions()
      
      num_vars = 0
      num_restart_vars = 0
      if (allocated(model_vars)) deallocate(model_vars)
      allocate(model_vars(MAX_VARS))

#define OUTVAR(vname, long, units, vgrid, nd, var, is_rst) call define_var(vname, long, units, vgrid, nd, is_rst);
#include "output_vars.inc"
#undef OUTVAR

      print *, "Initialized ", num_vars, " output variables from output_vars.inc"
      print *, "  Restart variables: ", num_restart_vars

   end subroutine init_var_definitions

   !---------------------------------------------------------------------------
   !> @brief Send all output variables
   !>
   !> For INSTANT files: writes directly if it's time to output
   !> For AVERAGE files: accumulates in buffers
   !> NO memory duplication for instant outputs!
   !---------------------------------------------------------------------------
   subroutine send_all_outputs(current_time)
      real, intent(in) :: current_time

#define OUTVAR(vname, long, units, vgrid, nd, var, is_rst) call send_var(vname, var, current_time);
#include "output_vars.inc"
#undef OUTVAR

   end subroutine send_all_outputs

   !---------------------------------------------------------------------------
   !> @brief Define a variable (metadata only)
   !---------------------------------------------------------------------------
   subroutine define_var(name, long_name, units, var_grid, ndims, is_restart)
      character(len=*), intent(in) :: name, long_name, units
      type(grid), intent(in) :: var_grid
      integer, intent(in) :: ndims
      logical, intent(in) :: is_restart

      num_vars = num_vars + 1
      if (num_vars > MAX_VARS) then
         print *, "ERROR: Too many variables. Increase MAX_VARS."
         stop 1
      end if

      model_vars(num_vars)%meta%name = name
      model_vars(num_vars)%meta%long_name = long_name
      model_vars(num_vars)%meta%units = units
      model_vars(num_vars)%meta%var_grid = var_grid
      model_vars(num_vars)%meta%ndims = ndims
      model_vars(num_vars)%data%use_buffer = .true.

      ! Register as restart variable if needed
      if (is_restart) then
         call register_restart_var(name)
      end if

   end subroutine define_var

   !---------------------------------------------------------------------------
   !> @brief Send 0D (scalar) variable
   !>
   !> For INSTANT: writes directly to file (no buffer)
   !> For AVERAGE: accumulates in buffer
   !> For RESTART: stores in var_registry buffer (only if marked as restart)
   !---------------------------------------------------------------------------
   subroutine send_var_0d(name, val, current_time)
      character(len=*), intent(in) :: name
      real, intent(in) :: val
      real, intent(in) :: current_time
      type(io_variable), pointer :: var_ptr

      ! Store in var_registry ONLY for restart variables
      if (needs_restart_buffer(name)) then
         var_ptr => get_variable_ptr(name)
         if (associated(var_ptr)) call var_ptr%data%set(val)
      end if

      ! Process for instant files (direct write, no buffer)
      call process_var_instant(name, current_time, scalar=val)
      
      ! Process for average files (accumulate in buffer)
      call process_var_average(name, scalar=val)

   end subroutine send_var_0d

   !---------------------------------------------------------------------------
   !> @brief Send 1D variable
   !---------------------------------------------------------------------------
   subroutine send_var_1d(name, val, current_time)
      character(len=*), intent(in) :: name
      real, intent(in) :: val(:)
      real, intent(in) :: current_time
      type(io_variable), pointer :: var_ptr

      if (needs_restart_buffer(name)) then
         var_ptr => get_variable_ptr(name)
         if (associated(var_ptr)) call var_ptr%data%set(val)
      end if

      call process_var_instant(name, current_time, d1=val)
      call process_var_average(name, d1=val)

   end subroutine send_var_1d

   !---------------------------------------------------------------------------
   !> @brief Send 2D variable
   !---------------------------------------------------------------------------
   subroutine send_var_2d(name, val, current_time)
      character(len=*), intent(in) :: name
      real, intent(in) :: val(:,:)
      real, intent(in) :: current_time
      type(io_variable), pointer :: var_ptr

      if (needs_restart_buffer(name)) then
         var_ptr => get_variable_ptr(name)
         if (associated(var_ptr)) call var_ptr%data%set(val)
      end if

      call process_var_instant(name, current_time, d2=val)
      call process_var_average(name, d2=val)

   end subroutine send_var_2d

   !---------------------------------------------------------------------------
   !> @brief Send 3D variable
   !---------------------------------------------------------------------------
   subroutine send_var_3d(name, val, current_time)
      character(len=*), intent(in) :: name
      real, intent(in) :: val(:,:,:)
      real, intent(in) :: current_time
      type(io_variable), pointer :: var_ptr

      if (needs_restart_buffer(name)) then
         var_ptr => get_variable_ptr(name)
         if (associated(var_ptr)) call var_ptr%data%set(val)
      end if

      call process_var_instant(name, current_time, d3=val)
      call process_var_average(name, d3=val)

   end subroutine send_var_3d

end module var_definitions
