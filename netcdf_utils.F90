!===============================================================================
!> @file netcdf_utils.F90
!>
!> NetCDF utility functions
!>
!> This module provides utility functions for NetCDF operations,
!> particularly error checking and handling.
!>
!> REFACTORED: Integrated with io_error module for consistent error handling.
!>
!> @author Rachid Benshila
!> @date 2025-04-11
!===============================================================================
module netcdf_utils
   use netcdf
   use io_error, only: io_error_check, IO_ERR_FILE, IO_ERR_VAR, IO_ERR_DIM, &
                       IO_ERR_WRITE, IO_ERR_READ
   implicit none
   private

   public :: nc_check, nc_check_status

contains

   !> Check NetCDF status and report error if needed
   !>
   !> This subroutine checks a NetCDF status code and reports any errors
   !> through the centralized error handling system.
   !>
   !> @param[in] status     NetCDF status code
   !> @param[in] operation  Optional: description of the operation being performed
   subroutine nc_check(status, operation)
      integer, intent(in) :: status
      character(len=*), intent(in), optional :: operation

      character(len=256) :: error_msg
      character(len=128) :: op_str
      logical :: is_ok

      if (status == nf90_noerr) return

      ! Build error message
      if (present(operation)) then
         op_str = operation
      else
         op_str = "NetCDF operation"
      end if

      error_msg = trim(op_str)//": "//trim(nf90_strerror(status))

      ! Map NetCDF error to our error code
      is_ok = io_error_check(map_nc_error(status), error_msg, "netcdf_utils")

   end subroutine nc_check

   !> Check NetCDF status and return success/failure
   !>
   !> This function checks a NetCDF status code, reports any errors,
   !> and returns whether the operation succeeded.
   !>
   !> @param[in]  status     NetCDF status code
   !> @param[in]  operation  Optional: description of the operation
   !> @param[out] error_msg  Optional: error message if failed
   !> @return     True if status indicates success, false otherwise
   function nc_check_status(status, operation, error_msg) result(is_ok)
      integer, intent(in) :: status
      character(len=*), intent(in), optional :: operation
      character(len=*), intent(out), optional :: error_msg
      logical :: is_ok

      character(len=256) :: msg
      character(len=128) :: op_str

      if (status == nf90_noerr) then
         is_ok = .true.
         if (present(error_msg)) error_msg = ""
         return
      end if

      is_ok = .false.

      ! Build error message
      if (present(operation)) then
         op_str = operation
      else
         op_str = "NetCDF operation"
      end if

      msg = trim(op_str)//": "//trim(nf90_strerror(status))

      if (present(error_msg)) then
         error_msg = msg
      end if

      ! Report through centralized error handling
      is_ok = io_error_check(map_nc_error(status), msg, "netcdf_utils")

   end function nc_check_status

   !> Map NetCDF error codes to I/O error codes
   !>
   !> @param[in] nc_status  NetCDF status code
   !> @return    Corresponding I/O error code
   function map_nc_error(nc_status) result(io_code)
      integer, intent(in) :: nc_status
      integer :: io_code

      select case (nc_status)
      ! File-related errors
      case (nf90_enotindefine, nf90_eindefine, nf90_eperm, nf90_enotatt, &
            nf90_enotnc, nf90_ebadid)
         io_code = IO_ERR_FILE

      ! Variable-related errors
      case (nf90_enotvar, nf90_einval, nf90_echar, nf90_eedge, nf90_estride)
         io_code = IO_ERR_VAR

      ! Dimension-related errors
      case (nf90_ebaddim, nf90_eunlimpos, nf90_emaxdims)
         io_code = IO_ERR_DIM

      ! Write errors
      case (nf90_erange, nf90_enomem)
         io_code = IO_ERR_WRITE

      ! Default
      case default
         io_code = IO_ERR_FILE
      end select
   end function map_nc_error

end module netcdf_utils
