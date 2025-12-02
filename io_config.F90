!===============================================================================
!> @file io_config.F90
!>
!> @brief I/O configuration management
!>
!> This module handles reading and applying I/O configuration from namelists.
!> The new file-centric approach defines output files and lists which variables
!> go into each file, rather than attaching output config to each variable.
!>
!> ## Namelist format
!>
!> ```fortran
!> &io_files_nml
!>    output_prefix = "ocean"
!>    time_units = "seconds since 2023-01-01"
!>    calendar = "gregorian"
!>    
!>    ! File 1: hourly instantaneous
!>    file_name(1) = "hourly"
!>    file_freq(1) = 3600.0
!>    file_operation(1) = "instant"
!>    file_vars(1) = "zeta,u,v"
!>    
!>    ! File 2: daily averages
!>    file_name(2) = "daily_avg"
!>    file_freq(2) = 86400.0
!>    file_operation(2) = "average"
!>    file_vars(2) = "zeta,temp"
!> /
!> ```
!>
!> @author Rachid Benshila
!> @date 2025-04
!===============================================================================
module io_config
   use io_constants, only: IO_PREFIX_LEN, IO_PATH_LEN, IO_VARNAME_LEN, &
                           IO_FREQ_DISABLED, IO_DEFAULT_PREFIX
   use io_file_registry, only: output_file_def, output_file_registry, &
                               OP_INSTANT, OP_AVERAGE, OP_MIN, OP_MAX, OP_ACCUMULATE
   implicit none
   private

   ! Public interface
   public :: read_io_config
   public :: get_output_prefix
   public :: get_time_units
   public :: get_calendar
   public :: file_registry

   ! Maximum files and variable string length
   integer, parameter :: MAX_FILES = 20
   integer, parameter :: MAX_VARLIST_LEN = 1024

   ! Global configuration
   character(len=IO_PREFIX_LEN), private :: output_prefix = IO_DEFAULT_PREFIX
   character(len=IO_PATH_LEN), private :: time_units = "seconds since 2023-01-01 00:00:00"
   character(len=64), private :: calendar = "gregorian"

   ! File registry (access controlled via public statement above)
   type(output_file_registry), target :: file_registry

contains

   !---------------------------------------------------------------------------
   !> @brief Read I/O configuration from namelist file
   !>
   !> @param[in] filename  Path to namelist file
   !> @return    Status (0 = success, -1 = file not found, -2 = read error)
   !---------------------------------------------------------------------------
   function read_io_config(filename) result(status)
      character(len=*), intent(in) :: filename
      integer :: status

      ! Namelist variables
      character(len=IO_PREFIX_LEN) :: nml_output_prefix
      character(len=IO_PATH_LEN) :: nml_time_units
      character(len=64) :: nml_calendar

      character(len=64) :: file_name(MAX_FILES)
      real :: file_freq(MAX_FILES)
      character(len=32) :: file_operation(MAX_FILES)
      character(len=MAX_VARLIST_LEN) :: file_vars(MAX_FILES)
      character(len=IO_PREFIX_LEN) :: file_prefix(MAX_FILES)

      namelist /io_files_nml/ nml_output_prefix, nml_time_units, nml_calendar, &
                              file_name, file_freq, file_operation, file_vars, file_prefix

      integer :: iunit, ios, i
      type(output_file_def) :: file_def

      status = 0

      ! Initialize defaults
      nml_output_prefix = IO_DEFAULT_PREFIX
      nml_time_units = "seconds since 2023-01-01 00:00:00"
      nml_calendar = "gregorian"
      file_name = ""
      file_freq = IO_FREQ_DISABLED
      file_operation = "instant"
      file_vars = ""
      file_prefix = ""

      ! Open and read namelist
      open(newunit=iunit, file=filename, status='old', action='read', iostat=ios)
      if (ios /= 0) then
         print *, "Warning: Could not open config file: ", trim(filename)
         print *, "Using default configuration"
         call setup_default_config()
         status = -1
         return
      end if

      read(iunit, nml=io_files_nml, iostat=ios)
      close(iunit)

      if (ios /= 0) then
         print *, "Warning: Error reading namelist io_files_nml, iostat=", ios
         print *, "Using default configuration"
         call setup_default_config()
         status = -2
         return
      end if

      ! Store global settings
      output_prefix = nml_output_prefix
      time_units = nml_time_units
      calendar = nml_calendar

      ! Process each file definition
      do i = 1, MAX_FILES
         if (trim(file_name(i)) == "") cycle
         if (file_freq(i) <= 0.0) cycle

         ! Create file definition
         file_def%name = file_name(i)
         file_def%frequency = file_freq(i)
         file_def%operation = parse_operation(file_operation(i))
         
         if (trim(file_prefix(i)) /= "") then
            file_def%prefix = file_prefix(i)
         else
            file_def%prefix = output_prefix
         end if

         ! Parse variable list
         call parse_variable_list(file_vars(i), file_def)

         ! Initialize averaging states if needed
         call file_def%init_avg_states()

         ! Add to registry
         call file_registry%add(file_def)

         print *, "Configured file: ", trim(file_def%name), &
                  " freq=", file_def%frequency, &
                  " op=", file_def%operation, &
                  " nvars=", file_def%num_variables
      end do

      print *, "Loaded ", file_registry%size(), " output file definitions"

   end function read_io_config

   !---------------------------------------------------------------------------
   !> @brief Setup default configuration when no config file
   !---------------------------------------------------------------------------
   subroutine setup_default_config()
      type(output_file_def) :: file_def

      ! Default: one history file with all common variables
      file_def%name = "history"
      file_def%prefix = output_prefix
      file_def%frequency = 3600.0  ! Hourly
      file_def%operation = OP_INSTANT

      ! Add some default variables
      call file_def%add_variable("zeta")
      call file_def%add_variable("u")
      call file_def%add_variable("v")
      call file_def%add_variable("temp")

      call file_registry%add(file_def)
   end subroutine setup_default_config

   !---------------------------------------------------------------------------
   !> @brief Parse operation string to constant
   !---------------------------------------------------------------------------
   function parse_operation(op_str) result(op)
      character(len=*), intent(in) :: op_str
      integer :: op

      select case (trim(adjustl(op_str)))
      case ("instant", "INSTANT", "Instant")
         op = OP_INSTANT
      case ("average", "AVERAGE", "Average", "avg", "AVG", "mean", "MEAN")
         op = OP_AVERAGE
      case ("min", "MIN", "minimum", "MINIMUM")
         op = OP_MIN
      case ("max", "MAX", "maximum", "MAXIMUM")
         op = OP_MAX
      case ("accumulate", "ACCUMULATE", "sum", "SUM")
         op = OP_ACCUMULATE
      case default
         print *, "Warning: Unknown operation '", trim(op_str), "', using instant"
         op = OP_INSTANT
      end select
   end function parse_operation

   !---------------------------------------------------------------------------
   !> @brief Parse comma-separated variable list
   !---------------------------------------------------------------------------
   subroutine parse_variable_list(var_string, file_def)
      character(len=*), intent(in) :: var_string
      type(output_file_def), intent(inout) :: file_def

      character(len=IO_VARNAME_LEN) :: var_name
      integer :: i, start_pos, str_len

      file_def%num_variables = 0
      str_len = len_trim(var_string)
      if (str_len == 0) return

      start_pos = 1
      var_name = ""

      do i = 1, str_len
         if (var_string(i:i) == ',' .or. i == str_len) then
            ! Extract variable name
            if (i == str_len .and. var_string(i:i) /= ',') then
               var_name = adjustl(var_string(start_pos:i))
            else
               var_name = adjustl(var_string(start_pos:i-1))
            end if

            ! Add if non-empty
            if (len_trim(var_name) > 0) then
               call file_def%add_variable(trim(var_name))
            end if

            start_pos = i + 1
            var_name = ""
         end if
      end do
   end subroutine parse_variable_list

   !---------------------------------------------------------------------------
   !> @brief Get the global output prefix
   !---------------------------------------------------------------------------
   function get_output_prefix() result(prefix)
      character(len=IO_PREFIX_LEN) :: prefix
      prefix = output_prefix
   end function get_output_prefix

   !---------------------------------------------------------------------------
   !> @brief Get time units string
   !---------------------------------------------------------------------------
   function get_time_units() result(units)
      character(len=IO_PATH_LEN) :: units
      units = time_units
   end function get_time_units

   !---------------------------------------------------------------------------
   !> @brief Get calendar type
   !---------------------------------------------------------------------------
   function get_calendar() result(cal)
      character(len=64) :: cal
      cal = calendar
   end function get_calendar

end module io_config
