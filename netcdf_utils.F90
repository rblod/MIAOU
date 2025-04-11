module netcdf_utils
   use netcdf
   implicit none
   private
   public :: nc_check

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
   !> @param[status] ierr  NetCDF return code to check
   !> @param[in]     msg   error message
   !> @throws    Prints error message and stops execution if an error occurred  subroutine check_nc_status(status, msg)
   subroutine nc_check_status(status, msg)
      integer, intent(in) :: status
      character(len=*), intent(in) :: msg

      if (status /= nf90_noerr) then
         print *, "NetCDF error : ", trim(msg)
         print *, "Error code : ", status, " - ", trim(nf90_strerror(status))
         stop 1
      end if
   end subroutine nc_check_status

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

   !> Check NetCDF status and handle errors
   !>
   !> @param[in] ierr  NetCDF return code to check
   !> @throws    Prints error message and stops execution if an error occurred
   subroutine nc_check_status_basic(status)
      integer, intent(in) :: status
      if (status /= nf90_noerr) then
         print *, 'NetCDF error :', trim(nf90_strerror(status))
         stop 1
      end if
   end subroutine nc_check_status_basic

end module netcdf_utils
