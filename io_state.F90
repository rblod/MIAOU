!===============================================================================
!> @file io_state.F90
!>
!> @brief Runtime state for the MIAOU I/O system (v5.4.0)
!>
!> This module centralizes all runtime state that was previously scattered
!> across io_config and io_manager. This separation enables:
!>
!> - Cleaner unit testing (state can be reset between tests)
!> - Multiple simultaneous configurations (future: multi-grid support)
!> - Clear distinction between "what config says" vs "what's currently loaded"
!>
!> ## Contained State
!>
!> - `file_registry` — Registered output files
!> - `var_registry` — Registered model variables
!> - `io_initialized` — Whether I/O system is ready
!>
!> ## Usage
!>
!> ```fortran
!> use io_state, only: file_registry, var_registry, reset_io_state
!>
!> ! Normal usage - registries are populated by io_config and io_manager
!> call file_registry%add(file_def)
!> call var_registry%add(var)
!>
!> ! For testing - reset all state
!> call reset_io_state()
!> ```
!>
!> @author Rachid Benshila
!> @date 2025-04
!> @version 5.4.0
!===============================================================================
module io_state
   use io_file_registry, only: output_file_registry
   use io_definitions, only: io_var_registry
   implicit none

   !---------------------------------------------------------------------------
   ! Public interface
   !---------------------------------------------------------------------------
   public :: file_registry
   public :: var_registry
   public :: io_initialized, files_created
   public :: reset_io_state

   !---------------------------------------------------------------------------
   ! Runtime state - file registry
   !>
   !> Contains all configured output files (from namelist parsing).
   !> Populated by io_config%read_io_config().
   !---------------------------------------------------------------------------
   type(output_file_registry), target :: file_registry

   !---------------------------------------------------------------------------
   ! Runtime state - variable registry
   !>
   !> Contains all registered model variables.
   !> Populated by io_manager%register_variable().
   !---------------------------------------------------------------------------
   type(io_var_registry), target :: var_registry

   !---------------------------------------------------------------------------
   ! Initialization flags
   !---------------------------------------------------------------------------
   logical :: io_initialized = .false.   !< I/O system ready for use
   logical :: files_created = .false.    !< Output files have been created

contains

   !---------------------------------------------------------------------------
   !> @brief Reset all I/O state (for testing)
   !>
   !> Clears both registries and resets initialization flags.
   !> Use this between test cases to ensure clean state.
   !---------------------------------------------------------------------------
   subroutine reset_io_state()
      ! Clear registries properly
      call file_registry%clear()
      call var_registry%clear()
      
      ! Reset flags
      io_initialized = .false.
      files_created = .false.
      
   end subroutine reset_io_state

end module io_state
