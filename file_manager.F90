!===============================================================================
!> @file file_manager.F90
!>
!> Enhanced output manager supporting multiple dimensions (0D, 1D, 2D, 3D)
!>
!> This module provides a comprehensive system for managing NetCDF output files
!> for variables of different dimensions. The implementation is distributed
!> across several submodules for better organization.
!>
!> @author Rachid Benshila
!> @date 2025-04-10
!===============================================================================
module file_manager
   use netcdf
   use grid_module
   use namelist_output, only: his_prefix, avg_prefix, rst_prefix
   use netcdf_backend, only: nc_check, nc_define_variable, nc_write_variable

   implicit none
   private

   ! Public interface
   public :: register_variable, define_output_file, write_output, generate_filename
   public :: initialize_output_files, finalize_output, write_all_outputs
   public :: nc_var

   !> @type nc_var
   !> Enhanced variable type supporting multiple dimensions
   !>
   !> This type represents a model variable that can be output to NetCDF files.
   !> It supports scalar (0D), vector (1D), matrix (2D), and volume (3D) data
   !> with configurable output settings for history, average, and restart files.
   type :: nc_var
      character(len=32) :: name            !< Variable name
      character(len=64) :: long_name       !< Long descriptive name
      character(len=32) :: units           !< Variable units
      type(grid) :: var_grid               !< Grid associated with the variable

      !< Dimension count (0=scalar, 1=1D, 2=2D, 3=3D)
      integer :: ndims = 0

      !< NetCDF variable IDs for different file types
      integer :: varid_his = -1            !< ID in history file
      integer :: varid_avg = -1            !< ID in average file
      integer :: varid_rst = -1            !< ID in restart file

      !< Output flags
      logical :: to_his = .false.          !< Write to history file
      logical :: to_avg = .false.          !< Write to average file
      logical :: to_rst = .false.          !< Write to restart file

      !< Data pointers for different dimensions - only one will be associated
      real, pointer :: scalar => null()             !< 0D data
      real, pointer :: data_1d(:) => null()         !< 1D data
      real, pointer :: data_2d(:, :) => null()      !< 2D data
      real, pointer :: data_3d(:, :, :) => null()   !< 3D data

      !< Averaging buffers with matching dimension
      real :: scalar_avg = 0.0                      !< For 0D
      real, allocatable :: avg_buffer_1d(:)         !< For 1D
      real, allocatable :: avg_buffer_2d(:, :)      !< For 2D
      real, allocatable :: avg_buffer_3d(:, :, :)   !< For 3D

      !< Time and frequency settings
      integer :: current_time_index = 1              !< Current time index
      real :: freq_his = -1.                         !< Frequency for history (seconds)
      real :: freq_avg = -1.                         !< Frequency for average (seconds)
      real :: freq_rst = -1.                         !< Frequency for restart (seconds)
      character(len=128) :: file_prefix = ""         !< Custom file prefix

      !< Memory management flag
      logical :: owns_avg_buffers = .false.          !< Indicates if this instance owns its buffers

   contains
      !< Allocate average buffers if needed
      procedure :: allocate_buffers

      !< Free average buffers if the variable owns them
      procedure :: deallocate_buffers

      !< Reset average buffers to zero without deallocating
      procedure :: reset_avg_buffers

   end type nc_var

   !> @type output_file
   !> Structure for tracking open NetCDF files
   !>
   !> This type maintains information about each open output file,
   !> including its type, frequency, and current time index.
   type :: output_file
      character(len=256) :: filename  !< Complete filename
      character(len=16) :: type       !< "his", "avg", or "rst"
      real :: freq                    !< Write frequency (seconds)
      integer :: ncid                 !< NetCDF file ID
      integer :: time_dimid           !< Time dimension ID
      integer :: time_varid           !< Time variable ID
      integer :: time_index = 1       !< Current time write index
   end type output_file

   !< Array of all registered variables
   type(nc_var), allocatable :: registered_vars(:)

   !< Array of all open output files
   type(output_file), allocatable :: open_files(:)

   ! Module procedure interfaces
   interface
      ! Variable buffer management submodule
      module subroutine allocate_buffers(this)
         class(nc_var), intent(inout) :: this
      end subroutine allocate_buffers

      module subroutine deallocate_buffers(this)
         class(nc_var), intent(inout) :: this
      end subroutine deallocate_buffers

      module subroutine reset_avg_buffers(this)
         class(nc_var), intent(inout) :: this
      end subroutine reset_avg_buffers

      ! Registration submodule
      module subroutine register_variable(v)
         type(nc_var), intent(in) :: v
      end subroutine register_variable

      module subroutine associate_variable_data(target_var, source_var)
         type(nc_var), intent(inout) :: target_var
         type(nc_var), intent(in) :: source_var
      end subroutine associate_variable_data

      ! File operations submodule
      module function generate_filename(var_prefix, file_type, freq) result(filename)
         character(len=*), intent(in) :: var_prefix, file_type
         real, intent(in) :: freq
         character(len=256) :: filename
      end function generate_filename

      module subroutine add_to_open_files(filename, file_type, freq, ncid)
         character(len=*), intent(in) :: filename, file_type
         real, intent(in) :: freq
         integer, intent(in) :: ncid
      end subroutine add_to_open_files

      module subroutine define_output_file(ncid, tag)
         integer, intent(in) :: ncid
         character(len=*), intent(in) :: tag
      end subroutine define_output_file

      module subroutine define_single_variable(ncid, tag, var_idx)
         integer, intent(in) :: ncid
         character(len=*), intent(in) :: tag
         integer, intent(in) :: var_idx
      end subroutine define_single_variable

      ! Writing submodule
      module subroutine write_output(ncid, tag, time_value)
         integer, intent(in) :: ncid
         character(len=*), intent(in) :: tag
         real, intent(in) :: time_value
      end subroutine write_output

      module subroutine write_all_outputs(current_time, is_final_step)
         real, intent(in) :: current_time
         logical, intent(in), optional :: is_final_step
      end subroutine write_all_outputs

      ! Initialization and cleanup submodule
      module subroutine initialize_output_files(time_units, calendar)
         character(len=*), intent(in), optional :: time_units, calendar
      end subroutine initialize_output_files

      module subroutine close_all_output_files()
      end subroutine close_all_output_files

      module subroutine cleanup_variable(var)
         type(nc_var), intent(inout) :: var
      end subroutine cleanup_variable

      module subroutine cleanup_all_variables()
      end subroutine cleanup_all_variables

      module subroutine finalize_output()
      end subroutine finalize_output
   end interface

end module file_manager