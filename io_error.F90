!===============================================================================
!> @file io_error.F90
!>
!> Centralized error handling for the I/O system
!>
!> This module provides consistent error handling across all I/O modules,
!> including error codes, messages, and configurable behavior (continue vs stop).
!>
!> @author Rachid Benshila
!> @date 2025-04-11
!===============================================================================
module io_error
   implicit none
   private

   ! Public interface
   public :: io_error_init, io_error_set_strict
   public :: io_error_check, io_error_report
   public :: io_error_get_last, io_error_clear
   public :: IO_SUCCESS, IO_ERR_FILE, IO_ERR_VAR, IO_ERR_DIM
   public :: IO_ERR_WRITE, IO_ERR_READ, IO_ERR_CONFIG, IO_ERR_INTERNAL

   ! Error codes
   integer, parameter :: IO_SUCCESS      = 0    !< No error
   integer, parameter :: IO_ERR_FILE     = -1   !< File operation error
   integer, parameter :: IO_ERR_VAR      = -2   !< Variable operation error
   integer, parameter :: IO_ERR_DIM      = -3   !< Dimension operation error
   integer, parameter :: IO_ERR_WRITE    = -4   !< Write operation error
   integer, parameter :: IO_ERR_READ     = -5   !< Read operation error
   integer, parameter :: IO_ERR_CONFIG   = -6   !< Configuration error
   integer, parameter :: IO_ERR_INTERNAL = -99  !< Internal/unexpected error

   ! Module state
   logical, private :: strict_mode = .false.     !< Stop on error if true
   logical, private :: verbose_mode = .true.     !< Print errors if true
   integer, private :: last_error_code = 0
   character(len=512), private :: last_error_msg = ""
   character(len=128), private :: last_error_location = ""

contains

   !> Initialize the error handling system
   !>
   !> @param[in] strict   Optional: enable strict mode (stop on error)
   !> @param[in] verbose  Optional: enable verbose mode (print errors)
   subroutine io_error_init(strict, verbose)
      logical, intent(in), optional :: strict, verbose

      if (present(strict)) strict_mode = strict
      if (present(verbose)) verbose_mode = verbose

      call io_error_clear()
   end subroutine io_error_init

   !> Set strict mode
   !>
   !> @param[in] strict  Enable or disable strict mode
   subroutine io_error_set_strict(strict)
      logical, intent(in) :: strict
      strict_mode = strict
   end subroutine io_error_set_strict

   !> Check a status and handle error if needed
   !>
   !> @param[in] status    Status code to check (0 = success)
   !> @param[in] message   Error message if status indicates error
   !> @param[in] location  Optional: location in code (module/subroutine)
   !> @return    True if status indicates success, false otherwise
   function io_error_check(status, message, location) result(is_ok)
      integer, intent(in) :: status
      character(len=*), intent(in) :: message
      character(len=*), intent(in), optional :: location
      logical :: is_ok

      if (status == 0) then
         is_ok = .true.
         return
      end if

      is_ok = .false.

      ! Store error information
      last_error_code = status
      last_error_msg = message
      if (present(location)) then
         last_error_location = location
      else
         last_error_location = ""
      end if

      ! Report error
      call io_error_report(status, message, location)

      ! Stop if in strict mode
      if (strict_mode) then
         print *, "FATAL: Stopping due to I/O error (strict mode enabled)"
         stop 1
      end if
   end function io_error_check

   !> Report an error (print to stdout)
   !>
   !> @param[in] code      Error code
   !> @param[in] message   Error message
   !> @param[in] location  Optional: location in code
   subroutine io_error_report(code, message, location)
      integer, intent(in) :: code
      character(len=*), intent(in) :: message
      character(len=*), intent(in), optional :: location

      character(len=16) :: code_str
      character(len=32) :: severity

      if (.not. verbose_mode) return

      ! Determine severity based on code
      select case (code)
      case (IO_SUCCESS)
         return  ! Don't report success
      case (IO_ERR_CONFIG)
         severity = "WARNING"
      case (IO_ERR_INTERNAL)
         severity = "INTERNAL ERROR"
      case default
         severity = "ERROR"
      end select

      write(code_str, '(I0)') code

      ! Print error message
      if (present(location) .and. len_trim(location) > 0) then
         print '(A,": [",A,"] ",A," (in ",A,")")', &
            trim(severity), trim(code_str), trim(message), trim(location)
      else
         print '(A,": [",A,"] ",A)', &
            trim(severity), trim(code_str), trim(message)
      end if
   end subroutine io_error_report

   !> Get information about the last error
   !>
   !> @param[out] code      Error code
   !> @param[out] message   Error message
   !> @param[out] location  Location where error occurred
   subroutine io_error_get_last(code, message, location)
      integer, intent(out), optional :: code
      character(len=*), intent(out), optional :: message
      character(len=*), intent(out), optional :: location

      if (present(code)) code = last_error_code
      if (present(message)) message = last_error_msg
      if (present(location)) location = last_error_location
   end subroutine io_error_get_last

   !> Clear the last error
   subroutine io_error_clear()
      last_error_code = 0
      last_error_msg = ""
      last_error_location = ""
   end subroutine io_error_clear

end module io_error
