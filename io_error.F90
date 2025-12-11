!===============================================================================
!> @file io_error.F90
!>
!> @brief Centralized error handling for the MIAOU I/O system
!>
!> This module provides consistent error handling across all I/O modules,
!> including error codes, messages, and configurable behavior (continue vs stop).
!>
!> ## Usage
!>
!> All MIAOU modules should use this module for error reporting:
!>
!> ```fortran
!> use io_error
!> integer :: ierr
!>
!> ! Check a NetCDF status
!> ierr = io_check_nc(nf90_status, "Failed to create file", "my_subroutine")
!> if (ierr /= IO_SUCCESS) return
!>
!> ! Report a custom error
!> call io_report_error(IO_ERR_CONFIG, "Invalid frequency", "read_config")
!>
!> ! Check with automatic return code
!> if (.not. io_error_check(status, "message")) then
!>    status = IO_ERR_FILE
!>    return
!> end if
!> ```
!>
!> ## Error Handling Strategy
!>
!> - All functions return integer status codes (0 = success)
!> - In strict mode, errors cause immediate program termination
!> - In normal mode, errors are logged and returned to caller
!> - Error count is tracked for summary reporting
!>
!> @author Rachid Benshila
!> @date 2025-04
!> @version 5.3.0
!===============================================================================
module io_error
   implicit none
   private

   !---------------------------------------------------------------------------
   ! Public interface
   !---------------------------------------------------------------------------
   public :: io_error_init, io_error_finalize
   public :: io_error_set_strict, io_error_set_verbose
   public :: io_error_check, io_check_nc
   public :: io_report_error, io_report_warning
   public :: io_error_get_last, io_error_clear
   public :: io_error_get_count, io_error_get_warning_count

   !---------------------------------------------------------------------------
   ! Error codes - organized by category
   !---------------------------------------------------------------------------
   
   ! Success
   integer, public, parameter :: IO_SUCCESS = 0           !< No error
   
   ! File operations (1-19)
   integer, public, parameter :: IO_ERR_FILE = 1          !< General file error
   integer, public, parameter :: IO_ERR_FILE_CREATE = 2   !< Cannot create file
   integer, public, parameter :: IO_ERR_FILE_OPEN = 3     !< Cannot open file
   integer, public, parameter :: IO_ERR_FILE_CLOSE = 4    !< Cannot close file
   integer, public, parameter :: IO_ERR_FILE_EXISTS = 5   !< File already exists
   integer, public, parameter :: IO_ERR_FILE_NOTFOUND = 6 !< File not found
   
   ! Variable operations (20-39)
   integer, public, parameter :: IO_ERR_VAR = 20          !< General variable error
   integer, public, parameter :: IO_ERR_VAR_NOTFOUND = 21 !< Variable not found
   integer, public, parameter :: IO_ERR_VAR_EXISTS = 22   !< Variable already exists
   integer, public, parameter :: IO_ERR_VAR_TYPE = 23     !< Variable type mismatch
   integer, public, parameter :: IO_ERR_VAR_SHAPE = 24    !< Variable shape mismatch
   
   ! Dimension operations (40-59)
   integer, public, parameter :: IO_ERR_DIM = 40          !< General dimension error
   integer, public, parameter :: IO_ERR_DIM_NOTFOUND = 41 !< Dimension not found
   integer, public, parameter :: IO_ERR_DIM_MISMATCH = 42 !< Dimension size mismatch
   
   ! I/O operations (60-79)
   integer, public, parameter :: IO_ERR_WRITE = 60        !< Write operation error
   integer, public, parameter :: IO_ERR_READ = 61         !< Read operation error
   integer, public, parameter :: IO_ERR_SYNC = 62         !< Sync/flush error
   
   ! Configuration (80-99)
   integer, public, parameter :: IO_ERR_CONFIG = 80       !< Configuration error
   integer, public, parameter :: IO_ERR_CONFIG_PARSE = 81 !< Namelist parse error
   integer, public, parameter :: IO_ERR_CONFIG_VALUE = 82 !< Invalid config value
   integer, public, parameter :: IO_ERR_CONFIG_MISSING = 83 !< Missing config
   
   ! Memory/allocation (100-119)
   integer, public, parameter :: IO_ERR_ALLOC = 100       !< Allocation failed
   integer, public, parameter :: IO_ERR_DEALLOC = 101     !< Deallocation failed
   integer, public, parameter :: IO_ERR_LIMIT = 102       !< Limit exceeded
   
   ! MPI (120-139)
   integer, public, parameter :: IO_ERR_MPI = 120         !< General MPI error
   integer, public, parameter :: IO_ERR_MPI_COMM = 121    !< MPI communication error
   integer, public, parameter :: IO_ERR_MPI_SYNC = 122    !< MPI synchronization error
   
   ! Internal (900+)
   integer, public, parameter :: IO_ERR_INTERNAL = 900    !< Internal/unexpected error
   integer, public, parameter :: IO_ERR_NOT_IMPL = 901    !< Not implemented
   integer, public, parameter :: IO_ERR_INVALID_ARG = 902 !< Invalid argument

   !---------------------------------------------------------------------------
   ! Module state
   !---------------------------------------------------------------------------
   logical, save :: initialized = .false.
   logical, save :: strict_mode = .false.     !< Stop on error if true
   logical, save :: verbose_mode = .true.     !< Print errors if true
   integer, save :: error_count = 0           !< Total errors encountered
   integer, save :: warning_count = 0         !< Total warnings encountered
   integer, save :: last_error_code = 0
   character(len=512), save :: last_error_msg = ""
   character(len=128), save :: last_error_location = ""

contains

   !===========================================================================
   ! Initialization / Finalization
   !===========================================================================

   !---------------------------------------------------------------------------
   !> @brief Initialize the error handling system
   !>
   !> @param[in] strict   Optional: enable strict mode (stop on error)
   !> @param[in] verbose  Optional: enable verbose mode (print errors)
   !---------------------------------------------------------------------------
   subroutine io_error_init(strict, verbose)
      logical, intent(in), optional :: strict, verbose

      if (present(strict)) strict_mode = strict
      if (present(verbose)) verbose_mode = verbose

      error_count = 0
      warning_count = 0
      call io_error_clear()
      initialized = .true.
   end subroutine io_error_init

   !---------------------------------------------------------------------------
   !> @brief Finalize error handling and print summary
   !>
   !> @param[in] print_summary  Optional: print error/warning summary
   !---------------------------------------------------------------------------
   subroutine io_error_finalize(print_summary)
      logical, intent(in), optional :: print_summary
      logical :: do_print

      do_print = .true.
      if (present(print_summary)) do_print = print_summary

      if (do_print .and. verbose_mode) then
         if (error_count > 0 .or. warning_count > 0) then
            print '(A)', "----------------------------------------"
            print '(A)', "MIAOU I/O Summary:"
            if (error_count > 0) then
               print '(A,I0,A)', "  Errors:   ", error_count, " (see log above)"
            end if
            if (warning_count > 0) then
               print '(A,I0)', "  Warnings: ", warning_count
            end if
            print '(A)', "----------------------------------------"
         end if
      end if

      initialized = .false.
   end subroutine io_error_finalize

   !===========================================================================
   ! Configuration
   !===========================================================================

   !---------------------------------------------------------------------------
   !> @brief Set strict mode
   !>
   !> @param[in] strict  Enable or disable strict mode
   !---------------------------------------------------------------------------
   subroutine io_error_set_strict(strict)
      logical, intent(in) :: strict
      strict_mode = strict
   end subroutine io_error_set_strict

   !---------------------------------------------------------------------------
   !> @brief Set verbose mode
   !>
   !> @param[in] verbose  Enable or disable verbose mode
   !---------------------------------------------------------------------------
   subroutine io_error_set_verbose(verbose)
      logical, intent(in) :: verbose
      verbose_mode = verbose
   end subroutine io_error_set_verbose

   !===========================================================================
   ! Error checking functions
   !===========================================================================

   !---------------------------------------------------------------------------
   !> @brief Check a status and handle error if needed
   !>
   !> Returns true if status == 0 (success), false otherwise.
   !> In strict mode, non-zero status causes program termination.
   !>
   !> @param[in] status    Status code to check (0 = success)
   !> @param[in] message   Error message if status indicates error
   !> @param[in] location  Optional: location in code (module/subroutine)
   !> @return    True if status indicates success, false otherwise
   !---------------------------------------------------------------------------
   function io_error_check(status, message, location) result(is_ok)
      integer, intent(in) :: status
      character(len=*), intent(in) :: message
      character(len=*), intent(in), optional :: location
      logical :: is_ok

      if (status == 0 .or. status == IO_SUCCESS) then
         is_ok = .true.
         return
      end if

      is_ok = .false.
      call store_error(status, message, location)
      call print_error(status, message, location, "ERROR")
      
      error_count = error_count + 1

      if (strict_mode) then
         print '(A)', "FATAL: Stopping due to I/O error (strict mode enabled)"
         stop 1
      end if
   end function io_error_check

   !---------------------------------------------------------------------------
   !> @brief Check NetCDF status and convert to MIAOU error code
   !>
   !> This function checks a NetCDF status code and returns IO_SUCCESS if
   !> the operation succeeded, or an appropriate MIAOU error code otherwise.
   !>
   !> @param[in] nc_status  NetCDF status code (from nf90_* functions)
   !> @param[in] message    Error message for logging
   !> @param[in] location   Optional: location in code
   !> @return    IO_SUCCESS if nc_status == NF90_NOERR, error code otherwise
   !---------------------------------------------------------------------------
   function io_check_nc(nc_status, message, location) result(ierr)
      integer, intent(in) :: nc_status
      character(len=*), intent(in) :: message
      character(len=*), intent(in), optional :: location
      integer :: ierr
      
      character(len=256) :: full_message
      
      ! NF90_NOERR = 0 in NetCDF
      if (nc_status == 0) then
         ierr = IO_SUCCESS
         return
      end if
      
      ! Convert NetCDF error to MIAOU error
      ! We use IO_ERR_FILE as generic NetCDF error
      ierr = IO_ERR_FILE
      
      ! Build message with NetCDF error info
      ! Note: In real usage, you'd call nf90_strerror(nc_status)
      write(full_message, '(A," (NetCDF error code: ",I0,")")') trim(message), nc_status
      
      call store_error(ierr, full_message, location)
      call print_error(ierr, full_message, location, "ERROR")
      
      error_count = error_count + 1

      if (strict_mode) then
         print '(A)', "FATAL: Stopping due to NetCDF error (strict mode enabled)"
         stop 1
      end if
   end function io_check_nc

   !===========================================================================
   ! Error reporting
   !===========================================================================

   !---------------------------------------------------------------------------
   !> @brief Report an error
   !>
   !> Use this when you want to report an error without checking a status.
   !>
   !> @param[in] code      Error code
   !> @param[in] message   Error message
   !> @param[in] location  Optional: location in code
   !---------------------------------------------------------------------------
   subroutine io_report_error(code, message, location)
      integer, intent(in) :: code
      character(len=*), intent(in) :: message
      character(len=*), intent(in), optional :: location

      call store_error(code, message, location)
      call print_error(code, message, location, "ERROR")
      
      error_count = error_count + 1

      if (strict_mode) then
         print '(A)', "FATAL: Stopping due to I/O error (strict mode enabled)"
         stop 1
      end if
   end subroutine io_report_error

   !---------------------------------------------------------------------------
   !> @brief Report a warning (non-fatal)
   !>
   !> Warnings are always logged but never cause program termination.
   !>
   !> @param[in] message   Warning message
   !> @param[in] location  Optional: location in code
   !---------------------------------------------------------------------------
   subroutine io_report_warning(message, location)
      character(len=*), intent(in) :: message
      character(len=*), intent(in), optional :: location

      call print_error(IO_SUCCESS, message, location, "WARNING")
      warning_count = warning_count + 1
   end subroutine io_report_warning

   !===========================================================================
   ! Error information retrieval
   !===========================================================================

   !---------------------------------------------------------------------------
   !> @brief Get information about the last error
   !>
   !> @param[out] code      Error code
   !> @param[out] message   Error message
   !> @param[out] location  Location where error occurred
   !---------------------------------------------------------------------------
   subroutine io_error_get_last(code, message, location)
      integer, intent(out), optional :: code
      character(len=*), intent(out), optional :: message
      character(len=*), intent(out), optional :: location

      if (present(code)) code = last_error_code
      if (present(message)) message = last_error_msg
      if (present(location)) location = last_error_location
   end subroutine io_error_get_last

   !---------------------------------------------------------------------------
   !> @brief Clear the last error
   !---------------------------------------------------------------------------
   subroutine io_error_clear()
      last_error_code = 0
      last_error_msg = ""
      last_error_location = ""
   end subroutine io_error_clear

   !---------------------------------------------------------------------------
   !> @brief Get total error count
   !---------------------------------------------------------------------------
   function io_error_get_count() result(count)
      integer :: count
      count = error_count
   end function io_error_get_count

   !---------------------------------------------------------------------------
   !> @brief Get total warning count
   !---------------------------------------------------------------------------
   function io_error_get_warning_count() result(count)
      integer :: count
      count = warning_count
   end function io_error_get_warning_count

   !===========================================================================
   ! Internal helper functions
   !===========================================================================

   !---------------------------------------------------------------------------
   !> @brief Store error information
   !---------------------------------------------------------------------------
   subroutine store_error(code, message, location)
      integer, intent(in) :: code
      character(len=*), intent(in) :: message
      character(len=*), intent(in), optional :: location

      last_error_code = code
      last_error_msg = message
      if (present(location)) then
         last_error_location = location
      else
         last_error_location = ""
      end if
   end subroutine store_error

   !---------------------------------------------------------------------------
   !> @brief Print error message
   !---------------------------------------------------------------------------
   subroutine print_error(code, message, location, severity)
      integer, intent(in) :: code
      character(len=*), intent(in) :: message
      character(len=*), intent(in), optional :: location
      character(len=*), intent(in) :: severity

      character(len=64) :: code_name

      if (.not. verbose_mode) return

      ! Get human-readable code name
      code_name = get_error_name(code)

      ! Print error message
      if (present(location) .and. len_trim(location) > 0) then
         print '(A,": [",A,"] ",A," (in ",A,")")', &
            trim(severity), trim(code_name), trim(message), trim(location)
      else
         print '(A,": [",A,"] ",A)', &
            trim(severity), trim(code_name), trim(message)
      end if
   end subroutine print_error

   !---------------------------------------------------------------------------
   !> @brief Get human-readable error code name
   !---------------------------------------------------------------------------
   function get_error_name(code) result(name)
      integer, intent(in) :: code
      character(len=64) :: name
      character(len=16) :: code_str

      select case (code)
      case (IO_SUCCESS);         name = "SUCCESS"
      case (IO_ERR_FILE);        name = "FILE"
      case (IO_ERR_FILE_CREATE); name = "FILE_CREATE"
      case (IO_ERR_FILE_OPEN);   name = "FILE_OPEN"
      case (IO_ERR_FILE_CLOSE);  name = "FILE_CLOSE"
      case (IO_ERR_VAR);         name = "VAR"
      case (IO_ERR_VAR_NOTFOUND); name = "VAR_NOTFOUND"
      case (IO_ERR_VAR_EXISTS);  name = "VAR_EXISTS"
      case (IO_ERR_DIM);         name = "DIM"
      case (IO_ERR_DIM_MISMATCH); name = "DIM_MISMATCH"
      case (IO_ERR_WRITE);       name = "WRITE"
      case (IO_ERR_READ);        name = "READ"
      case (IO_ERR_CONFIG);      name = "CONFIG"
      case (IO_ERR_CONFIG_VALUE); name = "CONFIG_VALUE"
      case (IO_ERR_ALLOC);       name = "ALLOC"
      case (IO_ERR_LIMIT);       name = "LIMIT"
      case (IO_ERR_MPI);         name = "MPI"
      case (IO_ERR_INTERNAL);    name = "INTERNAL"
      case (IO_ERR_NOT_IMPL);    name = "NOT_IMPLEMENTED"
      case (IO_ERR_INVALID_ARG); name = "INVALID_ARG"
      case default
         write(code_str, '(I0)') code
         name = "ERR_" // trim(code_str)
      end select
   end function get_error_name

end module io_error
