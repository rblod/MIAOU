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
                           IO_FREQ_DISABLED, IO_DEFAULT_PREFIX, &
                           io_compression_enabled, io_compression_level, &
                           io_flush_freq, io_verbose, IO_QUIET, IO_NORMAL, IO_DEBUG
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
   integer, parameter :: MAX_GROUPS = 10
   integer, parameter :: MAX_VARLIST_LEN = 1024

   ! Global configuration
   character(len=IO_PREFIX_LEN), private :: output_prefix = IO_DEFAULT_PREFIX
   character(len=IO_PATH_LEN), private :: time_units = "seconds since 2023-01-01 00:00:00"
   character(len=64), private :: calendar = "gregorian"

   !> Variable group definition
   type :: var_group_def
      character(len=32) :: name = ""
      character(len=MAX_VARLIST_LEN) :: variables = ""
   end type var_group_def

   !> Storage for variable groups
   type(var_group_def), private :: var_groups(MAX_GROUPS)
   integer, private :: num_groups = 0

   ! File registry 
   ! NOTE: file_registry is declared here for convenience.
   ! Future refactor could separate config parsing from runtime state.
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

      ! Namelist variables - global settings
      character(len=IO_PREFIX_LEN) :: nml_output_prefix
      character(len=IO_PATH_LEN) :: nml_time_units
      character(len=64) :: nml_calendar
      logical :: nml_compression
      integer :: nml_compression_level
      integer :: nml_flush_freq
      integer :: nml_verbose

      ! Variable groups: group_name(i), group_vars(i)
      character(len=32) :: group_name(MAX_GROUPS)
      character(len=MAX_VARLIST_LEN) :: group_vars(MAX_GROUPS)

      ! File definitions
      character(len=64) :: file_name(MAX_FILES)
      real :: file_freq(MAX_FILES)
      character(len=32) :: file_operation(MAX_FILES)
      character(len=MAX_VARLIST_LEN) :: file_vars(MAX_FILES)
      character(len=IO_PREFIX_LEN) :: file_prefix(MAX_FILES)
      
      ! Restart-specific options
      logical :: file_restart(MAX_FILES)
      integer :: file_restart_nlevels(MAX_FILES)
      logical :: file_double(MAX_FILES)

      namelist /io_files_nml/ nml_output_prefix, nml_time_units, nml_calendar, &
                              nml_compression, nml_compression_level, &
                              nml_flush_freq, nml_verbose, &
                              group_name, group_vars, &
                              file_name, file_freq, file_operation, file_vars, file_prefix, &
                              file_restart, file_restart_nlevels, file_double

      integer :: iunit, ios, i
      type(output_file_def) :: file_def
      character(len=MAX_VARLIST_LEN) :: expanded_vars

      status = 0

      ! Initialize defaults
      nml_output_prefix = IO_DEFAULT_PREFIX
      nml_time_units = "seconds since 2023-01-01 00:00:00"
      nml_calendar = "gregorian"
      nml_compression = .true.
      nml_compression_level = 4
      nml_flush_freq = 0
      nml_verbose = 1
      group_name = ""
      group_vars = ""
      file_name = ""
      file_freq = IO_FREQ_DISABLED
      file_operation = "instant"
      file_vars = ""
      file_prefix = ""
      file_restart = .false.
      file_restart_nlevels = 1
      file_double = .false.

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
      io_compression_enabled = nml_compression
      io_compression_level = max(0, min(9, nml_compression_level))  ! Clamp to 0-9
      io_flush_freq = max(0, nml_flush_freq)
      io_verbose = max(0, min(2, nml_verbose))  ! Clamp to 0-2
      
      if (io_verbose >= IO_NORMAL) then
         if (io_compression_enabled) then
            print *, "Compression enabled, level=", io_compression_level
         else
            print *, "Compression disabled"
         end if
         if (io_flush_freq > 0) then
            print *, "Flush every", io_flush_freq, "writes"
         end if
      end if

      ! Store variable groups
      num_groups = 0
      do i = 1, MAX_GROUPS
         if (trim(group_name(i)) /= "") then
            num_groups = num_groups + 1
            var_groups(num_groups)%name = group_name(i)
            var_groups(num_groups)%variables = group_vars(i)
            if (io_verbose >= IO_NORMAL) then
               print *, "Registered variable group: @", trim(group_name(i))
            end if
         end if
      end do

      ! Process each file definition
      do i = 1, MAX_FILES
         if (trim(file_name(i)) == "") cycle
         ! Allow freq <= 0 for restart files (means final only)
         if (file_freq(i) <= 0.0 .and. .not. file_restart(i)) cycle

         ! Create file definition
         file_def%name = file_name(i)
         file_def%frequency = file_freq(i)
         file_def%operation = parse_operation(file_operation(i))
         
         if (trim(file_prefix(i)) /= "") then
            file_def%prefix = file_prefix(i)
         else
            file_def%prefix = output_prefix
         end if
         
         ! Restart options
         file_def%is_restart = file_restart(i)
         file_def%restart_nlevels = file_restart_nlevels(i)
         file_def%double_precision = file_double(i)

         ! Expand groups in variable list and parse
         expanded_vars = expand_groups(file_vars(i))
         call parse_variable_list(expanded_vars, file_def)

         ! Initialize averaging states if needed
         call file_def%init_avg_states()
         
         ! Initialize restart buffer if needed
         if (file_def%is_restart) then
            call file_def%init_restart_buffer()
         end if

         ! Add to registry
         call file_registry%add(file_def)

         if (io_verbose >= IO_NORMAL) then
            if (file_def%is_restart) then
               print *, "Configured RESTART file: ", trim(file_def%name), &
                        " nlevels=", file_def%restart_nlevels, &
                        " double=", file_def%double_precision, &
                        " nvars=", file_def%num_variables
            else
               print *, "Configured file: ", trim(file_def%name), &
                        " freq=", file_def%frequency, &
                        " op=", file_def%operation, &
                        " nvars=", file_def%num_variables
            end if
         end if
      end do

      if (io_verbose >= IO_NORMAL) then
         print *, "Loaded ", file_registry%size(), " output file definitions"
      end if

   end function read_io_config

   !---------------------------------------------------------------------------
   !> @brief Expand group references (@groupname) in variable string
   !>
   !> Example: "@surface,temp" -> "zeta,u,v,temp" if group "surface" = "zeta,u,v"
   !---------------------------------------------------------------------------
   function expand_groups(var_string) result(expanded)
      character(len=*), intent(in) :: var_string
      character(len=MAX_VARLIST_LEN) :: expanded

      character(len=MAX_VARLIST_LEN) :: token
      character(len=32) :: group_ref
      integer :: i, j, start_pos, str_len
      logical :: found

      expanded = ""
      str_len = len_trim(var_string)
      if (str_len == 0) return

      start_pos = 1

      do i = 1, str_len
         if (var_string(i:i) == ',' .or. i == str_len) then
            ! Extract token
            if (i == str_len .and. var_string(i:i) /= ',') then
               token = adjustl(var_string(start_pos:i))
            else
               token = adjustl(var_string(start_pos:i-1))
            end if

            ! Check if it's a group reference
            if (len_trim(token) > 0) then
               if (token(1:1) == '@') then
                  ! Group reference - expand it
                  group_ref = trim(token(2:))
                  found = .false.
                  do j = 1, num_groups
                     if (trim(var_groups(j)%name) == trim(group_ref)) then
                        ! Append group variables
                        if (len_trim(expanded) > 0) then
                           expanded = trim(expanded) // "," // trim(var_groups(j)%variables)
                        else
                           expanded = trim(var_groups(j)%variables)
                        end if
                        found = .true.
                        exit
                     end if
                  end do
                  if (.not. found) then
                     print *, "Warning: Unknown variable group '@", trim(group_ref), "'"
                  end if
               else
                  ! Regular variable - append as-is
                  if (len_trim(expanded) > 0) then
                     expanded = trim(expanded) // "," // trim(token)
                  else
                     expanded = trim(token)
                  end if
               end if
            end if

            start_pos = i + 1
         end if
      end do
   end function expand_groups

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
