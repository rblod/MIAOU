!===============================================================================
!> @file io_config.F90
!>
!> I/O configuration management module
!>
!> This module handles the reading and parsing of I/O configuration settings
!> from namelist files. It provides the configuration interface for the I/O system
!> independent of the specific backend implementation.
!>
!> @author Rachid Benshila
!> @date 2025-04-11
!===============================================================================
module io_config
   use io_definitions, only: io_variable
   implicit none
   private

   ! Public functions and variables
   public :: read_io_config, apply_config_to_variable
   public :: get_output_prefix, get_global_freq, get_global_flag

   !> Configuration type for variable output settings
   !>
   !> This type defines all parameters needed to configure how a variable
   !> is written to different output file types.
   type, public :: var_config
      !> Variable name
      character(len=32)  :: name = ""

      !> Flag to enable/disable history file output
      logical :: wrt = .false.

      !> Flag to enable/disable average file output
      logical :: avg = .false.

      !> Flag to enable/disable restart file output
      logical :: rst = .false.

      !> Custom file prefix for this variable (if not using global prefix)
      character(len=128) :: file_prefix = ""

      !> Output frequency for history files (seconds)
      !> @note Negative value disables output
      real :: freq_his = -1.0

      !> Output frequency for average files (seconds)
      !> @note Negative value disables output
      real :: freq_avg = -1.0

      !> Output frequency for restart files (seconds)
      !> @note Negative value disables output
      real :: freq_rst = -1.0
   end type var_config

   !> Global configuration settings
   character(len=128), private :: output_prefix = "model"
   real, private :: global_freq_his = 3600.0
   real, private :: global_freq_avg = 7200.0
   real, private :: global_freq_rst = 86400.0
   logical, private :: global_to_his = .true.
   logical, private :: global_to_avg = .false.
   logical, private :: global_to_rst = .true.

   !> Array of variable configurations read from namelist
   type(var_config), allocatable, private :: var_configs(:)

   !> Constants for error handling
   integer, parameter :: SUCCESS = 0
   integer, parameter :: FILE_NOT_FOUND = -1
   integer, parameter :: NAMELIST_ERROR = -2

   !> Namelist declarations
   namelist /output_global/ output_prefix, &
      global_freq_his, global_freq_avg, global_freq_rst, &
      global_to_his, global_to_avg, global_to_rst
   namelist /output_vars/ var_configs

contains

!> Read output configuration from namelist file
!>
!> This function reads the output configuration from a namelist file,
!> including global settings and variable-specific output settings.
!> It pre-allocates the configuration array to avoid issues with
!> indexed namelist entries.
!>
!> @param[in] filename  Path to the namelist file (optional, defaults to "output_config.nml")
!> @return    Status code (0 = success)
   function read_io_config(filename) result(status)
      character(len=*), intent(in), optional :: filename
      integer :: status
      character(len=256) :: config_file
      integer :: unit_id, ios, i, count
      character(len=256) :: error_msg
      type(var_config), allocatable :: temp(:)

      ! Set default filename if not provided
      if (present(filename)) then
         config_file = filename
      else
         config_file = "output_config.nml"
      end if

      ! Free memory if already allocated
      if (allocated(var_configs)) deallocate (var_configs)

      ! Get a free unit number
      unit_id = get_free_unit()

      ! Open the namelist file
      open (unit=unit_id, file=trim(config_file), status="old", action="read", iostat=ios)
      print *, "Reading configuration from file: ", trim(config_file)

      if (ios /= 0) then
         call handle_error(FILE_NOT_FOUND, "Cannot open "//trim(config_file), ios)
         status = FILE_NOT_FOUND
         allocate (var_configs(0))
         return
      end if

      ! Read global configuration parameters
      read (unit_id, nml=output_global, iostat=ios, iomsg=error_msg)
      if (ios /= 0) then
         call handle_error(NAMELIST_ERROR, "Error reading /output_global/", ios, error_msg)
         rewind (unit_id)
      end if
      print *, "Successfully read global configuration"
      print *, "  output_prefix = ", trim(output_prefix)
      print *, "  global_freq_his = ", global_freq_his
      print *, "  global_freq_avg = ", global_freq_avg
      print *, "  global_freq_rst = ", global_freq_rst
      ! Pre-allocate with a maximum expected size
      ! This avoids issues with indexed namelist entries
      allocate (var_configs(20))  ! Large enough for all expected variables

      ! Initialize to empty values to detect which entries were actually read
      do i = 1, size(var_configs)
         var_configs(i)%name = ""
      end do

      ! Read variable configurations
      rewind (unit_id)
      read (unit_id, nml=output_vars, iostat=ios, iomsg=error_msg)
      if (ios /= 0) then
         call handle_error(NAMELIST_ERROR, "Error reading /output_vars/", ios, error_msg)
      end if

! [après avoir lu var_configs...]
      if (allocated(var_configs)) then
         print *, "Read ", size(var_configs), " variable configurations:"
         do i = 1, size(var_configs)
            print *, "  Var ", i, ": name=", trim(var_configs(i)%name), &
               ", prefix='", trim(var_configs(i)%file_prefix), "'", &
               ", to_his=", var_configs(i)%wrt, &
               ", to_avg=", var_configs(i)%avg, &
               ", to_rst=", var_configs(i)%rst
         end do
      else
         print *, "ERROR: var_configs not allocated after reading!"
      end if

      ! Count how many entries were actually read
      count = 0
      do i = 1, size(var_configs)
         if (trim(var_configs(i)%name) /= "") then
            count = count + 1
         else
            exit
         end if
      end do

      ! Resize array to actual size
      if (count > 0 .and. count < size(var_configs)) then
         allocate (temp(count))
         temp(1:count) = var_configs(1:count)
         deallocate (var_configs)
         allocate (var_configs(count))
         var_configs = temp
         deallocate (temp)
      else if (count == 0) then
         ! No valid entries found, create an empty array
         deallocate (var_configs)
         allocate (var_configs(0))
      end if

      close (unit_id)
      status = SUCCESS
   end function read_io_config

   !> Apply configuration to a variable
   !>
   !> This function searches for matching configuration in the namelist data
   !> and applies it to the provided variable. If no matching configuration is found,
   !> global defaults are applied.
   !>
   !> @param[inout] var  The variable to configure
   subroutine apply_config_to_variable(var)
      type(io_variable), intent(inout) :: var
      integer :: i
      logical :: found

      found = .false.
      !  if(.not. allocated(var_configs)) stop

      ! Look for this variable in the configurations
      if (allocated(var_configs)) then
         do i = 1, size(var_configs)
            if (trim(var_configs(i)%name) == trim(var%name)) then
               ! Found - apply the configuration
               call apply_specific_config(var, var_configs(i))
               found = .true.

               print *, "Applied specific config for: ", trim(var%name), &
                  " file_prefix=", trim(var%file_prefix), &
                  " to_his=", var%to_his, " to_avg=", var%to_avg, " to_rst=", var%to_rst
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
   end subroutine apply_config_to_variable

   !> Apply specific configuration from namelist to a variable
   !>
   !> @param[inout] var        Target variable to configure
   !> @param[in]    var_conf   Configuration from namelist
   subroutine apply_specific_config(var, var_conf)
      type(io_variable), intent(inout) :: var
      type(var_config), intent(in) :: var_conf

      ! Copy output flags
      var%to_his = var_conf%wrt
      var%to_avg = var_conf%avg
      var%to_rst = var_conf%rst

      ! Apply global defaults for flags if not specified
      if (.not. var_conf%wrt .and. .not. var_conf%avg .and. .not. var_conf%rst) then
         ! If all flags are false, use global defaults
         var%to_his = global_to_his
         var%to_avg = global_to_avg
         var%to_rst = global_to_rst
      end if

      ! Copy output frequencies, using global defaults if unspecified
      ! For history files
      if (var_conf%freq_his > 0.0) then
         var%freq_his = var_conf%freq_his
      else if (var%to_his) then
         ! Use global default if flag is enabled
         var%freq_his = global_freq_his
      else
         var%freq_his = -1.0
      end if

      ! For average files
      if (var_conf%freq_avg > 0.0) then
         var%freq_avg = var_conf%freq_avg
      else if (var%to_avg) then
         ! Use global default if flag is enabled
         var%freq_avg = global_freq_avg
      else
         var%freq_avg = -1.0
      end if

      ! For restart files
      if (var_conf%freq_rst > 0.0) then
         var%freq_rst = var_conf%freq_rst
      else if (var%to_rst) then
         ! Use global default if flag is enabled
         var%freq_rst = global_freq_rst
      else
         var%freq_rst = -1.0
      end if

      ! Copy file prefix
      var%file_prefix = var_conf%file_prefix
   end subroutine apply_specific_config

   !> Get the global output prefix
   !>
   !> @return Current global output prefix
   function get_output_prefix() result(prefix)
      character(len=128) :: prefix
      prefix = output_prefix
   end function get_output_prefix

   !> Get a global frequency setting
   !>
   !> @param[in] file_type  Type of file ("his", "avg", or "rst")
   !> @return    Frequency in seconds
   function get_global_freq(file_type) result(freq)
      character(len=*), intent(in) :: file_type
      real :: freq

      select case (trim(file_type))
      case ("his")
         freq = global_freq_his
      case ("avg")
         freq = global_freq_avg
      case ("rst")
         freq = global_freq_rst
      case default
         freq = -1.0
      end select
   end function get_global_freq

   !> Get a global output flag
   !>
   !> @param[in] file_type  Type of file ("his", "avg", or "rst")
   !> @return    Whether output to this file type is enabled by default
   function get_global_flag(file_type) result(flag)
      character(len=*), intent(in) :: file_type
      logical :: flag

      select case (trim(file_type))
      case ("his")
         flag = global_to_his
      case ("avg")
         flag = global_to_avg
      case ("rst")
         flag = global_to_rst
      case default
         flag = .false.
      end select
   end function get_global_flag

   !> Get a free unit number for file operations
   !>
   !> @return A unit number that is not currently in use
   function get_free_unit() result(unit_id)
      integer :: unit_id
      logical :: is_open

      ! Start at 10 to avoid standard units
      unit_id = 10
      do
         inquire (unit=unit_id, opened=is_open)
         if (.not. is_open) return
         unit_id = unit_id + 1
      end do
   end function get_free_unit

!> Gestion centralisée des erreurs
!>
!> @param[in] error_code  Type d'erreur
!> @param[in] message     Message d'erreur
!> @param[in] iostatus    Code d'erreur I/O
!> @param[in] details     Détails supplémentaires optionnels
   subroutine handle_error(error_code, message, iostatus, details)
      integer, intent(in) :: error_code, iostatus
      character(len=*), intent(in) :: message
      character(len=*), intent(in), optional :: details

      ! Préfixe pour tous les messages d'erreur
      character(len=8) :: prefix

      ! Sélection du préfixe approprié en fonction du code d'erreur
      select case (error_code)
      case (FILE_NOT_FOUND)
         prefix = "WARNING"
      case (NAMELIST_ERROR)
         prefix = "WARNING"
      case default
         prefix = "ERROR"
      end select

      ! Affichage du message principal
      print '(A,": ",A," [Error code: ",I0,"]")', prefix, trim(message), iostatus

      ! Affichage des détails si présents
      if (present(details)) then
         print '(A,"     Details: ",A)', prefix, trim(details)
      end if

      ! Actions spécifiques selon le type d'erreur
      select case (error_code)
      case (FILE_NOT_FOUND)
         print *, "Using default values instead."
      case (NAMELIST_ERROR)
         ! Aucune action supplémentaire, le message a déjà été affiché
      case default
         ! Pour les erreurs critiques, on pourrait ajouter un stop ici
         ! stop 1
      end select
   end subroutine handle_error

end module io_config
