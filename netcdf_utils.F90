!===============================================================================
!> @file netcdf_utils.F90
!>
!> NetCDF utility functions
!>
!> This module provides utility functions for error checking and handling
!> in NetCDF operations.
!>
!> @author Rachid Benshila
!> @date 2025-04-11
!===============================================================================
module netcdf_utils
   use netcdf
   implicit none
   private
   public :: nc_check

   !> Generic interface for error checking NetCDF operations
   interface nc_check
      module procedure nc_check_status
      module procedure nc_check_status_soft
      module procedure nc_check_status_basic
   end interface

contains

   !---------------------------------------------------------------------------
   ! Error handling
   !---------------------------------------------------------------------------

   !> Check NetCDF status and handle errors
   !>
   !> @param[in] status  NetCDF return code to check
   !> @param[in] msg     Error message for context
   !> @throws    Prints error message and stops execution if an error occurred
   subroutine nc_check_status(status, msg)
      integer, intent(in) :: status
      character(len=*), intent(in) :: msg

      if (status /= nf90_noerr) then
         print *, "NetCDF error : ", trim(msg)
         print *, "Error code : ", status, " - ", trim(nf90_strerror(status))
         stop 1
      end if
   end subroutine nc_check_status

   !> Check NetCDF status with soft error handling
   !>
   !> @param[in]  status  NetCDF return code to check
   !> @param[in]  msg     Error message for context
   !> @param[out] success Flag indicating success (true) or failure (false)
   subroutine nc_check_status_soft(status, msg, success)
      integer, intent(in)  :: status
      character(len=*), intent(in)  :: msg
      logical, intent(out) :: success

      success = .true.
      if (status /= nf90_noerr) then
         print *, "Warning, NetCDF error : ", trim(msg)
         print *, "Error code : ", status, " - ", trim(nf90_strerror(status))
         success = .false.
      end if
   end subroutine nc_check_status_soft

   !> Check NetCDF status with basic error handling (no message)
   !>
   !> @param[in] status  NetCDF return code to check
   !> @throws    Prints error message and stops execution if an error occurred
   subroutine nc_check_status_basic(status)
      integer, intent(in) :: status
      if (status /= nf90_noerr) then
         print *, 'NetCDF error :', trim(nf90_strerror(status))
         stop 1
      end if
   end subroutine nc_check_status_basic

end module netcdf_utils
