!>
!> Module to read output configuration from a namelist file
!>
!> This module manages the reading of output configuration from namelist files,
!> handling both global output settings and individual variable output preferences.
!> It supports configuration for history files, average files, and restart files.
!>
!> @author Rachid Benshila
!> @date 2025-04-10
!>
module namelist_output
   implicit none
   private
   public :: his_prefix, avg_prefix, rst_prefix, var_output_config, dyn_vars
   public :: read_output_namelist

   !> @var his_prefix
   !> Prefix for history output files
   character(len=128) :: his_prefix = "history"

   !> @var avg_prefix
   !> Prefix for time average output files
   character(len=128) :: avg_prefix = "average"

   !> @var rst_prefix
   !> Prefix for restart files
   character(len=128) :: rst_prefix = "restart"

   !> Configuration type for variable output settings
   !>
   !> This type defines all parameters needed to configure how a variable
   !> is written to different output file types.
   !> @note Used directly in namelist input
   type :: var_output_config
      !> Variable name
      character(len=32)  :: name

      !> Flag to enable/disable history file output
      logical            :: wrt = .false.

      !> Flag to enable/disable average file output
      logical            :: avg = .false.

      !> Flag to enable/disable restart file output
      logical            :: rst = .false.

      !> Custom file prefix for this variable (if not using global prefix)
      character(len=128) :: file_prefix = ""

      !> Output frequency for history files (seconds)
      !> @note Negative value disables output
      real               :: freq_his = -1.0

      !> Output frequency for average files (seconds)
      !> @note Negative value disables output
      real               :: freq_avg = -1.0

      !> Output frequency for restart files (seconds)
      !> @note Negative value disables output
      real               :: freq_rst = -1.0
   end type var_output_config

   !> Array of variable output configurations read from namelist
   type(var_output_config), allocatable :: dyn_vars(:)

   ! Namelist declarations
   namelist /output_global/ his_prefix, avg_prefix, rst_prefix
   namelist /output_dyn/ dyn_vars

contains

   !> Read output configuration from namelist file
   !>
   !> This subroutine reads both global output settings and variable-specific
   !> output settings from the namelist file 'output_config.nml'.
   !> If the file cannot be read, default values are used.
   !>
   !> @note The namelist format must match the structure defined by the
   !>       namelist declarations in this module.
   subroutine read_output_namelist()
      integer :: ios, i, n_read  ! I/O status, loop index, and count
      type(var_output_config), allocatable :: tmp(:)  ! Temporary array for resizing

      integer, parameter :: maxvars = 100  ! Maximum number of variables supported in namelist

      ! Allocate and initialize the array with default values
      allocate (dyn_vars(maxvars))
      dyn_vars(:)%name = ""
      dyn_vars(:)%wrt = .false.
      dyn_vars(:)%avg = .false.
      dyn_vars(:)%rst = .false.
      dyn_vars(:)%file_prefix = ""
      dyn_vars(:)%freq_his = -1.0
      dyn_vars(:)%freq_avg = -1.0
      dyn_vars(:)%freq_rst = -1.0

      ! Open the namelist file
      open (unit=10, file="output_config.nml", status="old", action="read", iostat=ios)
      if (ios /= 0) then
         print *, "Warning: Cannot open output_config.nml, using defaults. Error code:", ios
         deallocate (dyn_vars)
         allocate (dyn_vars(0))
         return
      end if

      ! Read global configuration parameters
      read (10, nml=output_global, iostat=ios)
      if (ios /= 0) then
         print *, "Warning: Error reading namelist /output_global/, using defaults. Error code:", ios
         rewind (10)
      end if

      ! Read variable-specific output settings
      read (10, nml=output_dyn, iostat=ios)
      if (ios /= 0) then
         print *, "Warning: Error reading namelist /output_dyn/"
         if (allocated(dyn_vars)) deallocate (dyn_vars)
         allocate (dyn_vars(0))
         close (10)
         return
      end if

      ! Count how many variables were actually defined
      n_read = 0
      do i = 1, size(dyn_vars)
         if (trim(dyn_vars(i)%name) /= "") then
            n_read = n_read + 1
         else
            exit
         end if
      end do

      ! Resize array to exact size needed
      if (n_read > 0) then
         call move_alloc(dyn_vars, tmp)
         allocate (dyn_vars(n_read))
         dyn_vars = tmp(1:n_read)
      else
         call move_alloc(dyn_vars, tmp)
         allocate (dyn_vars(0))
      end if
   end subroutine read_output_namelist

end module namelist_output
