!===============================================================================
! file_manager.F90 : Generalized Output Manager with Grid-Based Dynamic Dimensions and Unlimited Time Axis
!===============================================================================
module file_manager
   use netcdf
   implicit none
   public :: register_variable, define_output_file, write_output, check

   type :: grid
      character(len=16) :: name                 ! e.g. "rho", "u", "v"
      character(len=32), allocatable :: dim_names(:)   ! e.g. ["xi_rho", "eta_rho", "s_rho", "time"]
      integer, allocatable :: dim_sizes(:)             ! e.g. [nx, ny, nz, 0]
      integer, allocatable :: dim_ids(:)               ! NetCDF IDs (assigned per file)
      logical :: has_time = .false.
   end type

   type :: nc_var
      character(len=32) :: name
      character(len=64) :: long_name
      character(len=32) :: units
      type(grid) :: var_grid
      integer :: varid_his = -1
      integer :: varid_avg = -1
      integer :: varid_rst = -1
      logical :: to_his = .false.
      logical :: to_avg = .false.
      logical :: to_rst = .false.
      real, pointer :: data(:, :) => null()
      real, allocatable :: avg_buffer(:, :)
      integer :: current_time_index = 1
      real :: freq = -1.
   end type

   type(nc_var), allocatable :: registered_vars(:)
   integer :: time_dimid = -1   ! Global time dimension ID
   integer :: time_varid = -1   ! Global time variable ID
   integer :: his_time_index = 1
   integer :: avg_time_index = 1
   integer :: rst_time_index = 1

contains

   subroutine register_variable(v)
      type(nc_var), intent(in) :: v
      type(nc_var), allocatable :: tmp(:)
      integer :: n

      if (.not. allocated(registered_vars)) then
         allocate (registered_vars(1))
         registered_vars(1) = v
         if (associated(v%data)) then
            registered_vars(1)%data => v%data
         else
            print *, "Warning: data not associated for variable ", trim(v%name)
         end if
      else
         n = size(registered_vars)
         allocate (tmp(n))
         tmp = registered_vars
         deallocate (registered_vars)
         allocate (registered_vars(n + 1))
         registered_vars(1:n) = tmp
         registered_vars(n + 1) = v
         if (associated(v%data)) then
            registered_vars(n + 1)%data => v%data
         else
            print *, "Warning: data not associated for variable ", trim(v%name)
         end if
      end if
   end subroutine register_variable

   subroutine define_output_file(ncid, tag, time_units, calendar)
      integer, intent(in) :: ncid
      character(len=*), intent(in) :: tag
      character(len=*), intent(in), optional :: time_units
      character(len=*), intent(in), optional :: calendar
      integer :: i, j, ncerr
      integer :: dim_id  ! Temporary variable to check dimension existence

      ! Reset time index for this file to be corrected not general
      his_time_index = 1
      avg_time_index = 1
      rst_time_index = 1

      ! Prepare time dimension for history and average files
      if (tag /= "rst") then
         ! Check if time dimension already exists
         ncerr = nf90_inq_dimid(ncid, "time", dim_id)
         if (ncerr /= nf90_noerr) then
            ! Time dimension doesn't exist, so define it
            ncerr = nf90_def_dim(ncid, "time", nf90_unlimited, time_dimid)
            call check(ncerr)

            if (present(time_units)) then
               ncerr = nf90_def_var(ncid, "time", nf90_real, [time_dimid], time_varid)
               call check(ncerr)

               if (present(calendar)) then
                  ncerr = nf90_put_att(ncid, time_varid, "units", trim(time_units))
                  call check(ncerr)
                  ncerr = nf90_put_att(ncid, time_varid, "calendar", trim(calendar))
                  call check(ncerr)
               end if
            end if
         end if
      end if

      ! Define dimensions and variables for registered variables
      do i = 1, size(registered_vars)
         select case (tag)
         case ("his")
            if (.not. registered_vars(i)%to_his) cycle
         case ("avg")
            if (.not. registered_vars(i)%to_avg) cycle
         case ("rst")
            if (.not. registered_vars(i)%to_rst) cycle
         end select

         ! Reset grid dimension IDs
         registered_vars(i)%var_grid%dim_ids = -1

         ! Define grid dimensions with uniqueness check
         do j = 1, size(registered_vars(i)%var_grid%dim_names)
            ! Check if dimension already exists
            ncerr = nf90_inq_dimid(ncid, &
                                   trim(registered_vars(i)%var_grid%dim_names(j)), &
                                   dim_id)

            if (ncerr /= nf90_noerr) then
               ! Dimension doesn't exist, define it
               ncerr = nf90_def_dim(ncid, &
                                    trim(registered_vars(i)%var_grid%dim_names(j)), &
                                    registered_vars(i)%var_grid%dim_sizes(j), &
                                    registered_vars(i)%var_grid%dim_ids(j))
               call check(ncerr)
            else
               ! Dimension exists, store its ID
               registered_vars(i)%var_grid%dim_ids(j) = dim_id
            end if
         end do

         ! Define variable based on tag
! Define variable based on tag
         select case (tag)
         case ("his")
            ncerr = nf90_def_var(ncid, registered_vars(i)%name, nf90_real, &
                                 [registered_vars(i)%var_grid%dim_ids(1:2), time_dimid], &
                                 registered_vars(i)%varid_his)
            call check(ncerr)
         case ("avg")
            ncerr = nf90_def_var(ncid, registered_vars(i)%name, nf90_real, &
                                 [registered_vars(i)%var_grid%dim_ids(1:2), time_dimid], &
                                 registered_vars(i)%varid_avg)
            if (.not. allocated(registered_vars(i)%avg_buffer)) then
               allocate (registered_vars(i)%avg_buffer(size(registered_vars(i)%data, 1),&
               &size(registered_vars(i)%data, 2)))
            end if

            call check(ncerr)
         case ("rst")
            ncerr = nf90_def_var(ncid, registered_vars(i)%name, nf90_real, &
                                 registered_vars(i)%var_grid%dim_ids(1:2), &
                                 registered_vars(i)%varid_rst)
            call check(ncerr)
         end select
      end do

   end subroutine define_output_file

   function find_grid_ids(target, pool) result(dim_ids)
      type(grid), intent(in) :: target
      type(grid), intent(in) :: pool(:)
      integer, allocatable :: dim_ids(:)
      integer :: i
      do i = 1, size(pool)
         if (target%name == pool(i)%name) then
            dim_ids = pool(i)%dim_ids
            return
         end if
      end do
      allocate (dim_ids(0))
   end function find_grid_ids

   subroutine define_one(ncid, v, vid)
      integer, intent(in) :: ncid
      type(nc_var), intent(in) :: v
      integer, intent(out) :: vid
      call check(nf90_def_var(ncid, v%name, nf90_real, v%var_grid%dim_ids, vid))
      call check(nf90_put_att(ncid, vid, "long_name", v%long_name))
      call check(nf90_put_att(ncid, vid, "units", v%units))
   end subroutine define_one

   subroutine write_output(ncid, tag, time_value)
      integer, intent(in) :: ncid
      character(len=*), intent(in) :: tag
      real, intent(in) :: time_value
      integer :: i
      integer, dimension(3) :: start, count
      real :: time_slice(1)

      do i = 1, size(registered_vars)
         select case (tag)
         case ("his")

            if (registered_vars(i)%to_his .and. &
            &    mod(time_value, registered_vars(i)%freq) == 0.) then

               start = [1, 1, his_time_index]
               count = [size(registered_vars(i)%data, 1), &
                        size(registered_vars(i)%data, 2), 1]

               call check(nf90_put_var(ncid, registered_vars(i)%varid_his, &
                                       registered_vars(i)%data, &
                                       start=start, count=count))
               his_time_index = his_time_index + 1
            end if

         case ("avg")
            if (registered_vars(i)%to_avg) then
               registered_vars(i)%avg_buffer = registered_vars(i)%avg_buffer + registered_vars(i)%data
               start = [1, 1, avg_time_index]
               count = [size(registered_vars(i)%data, 1), &
                        size(registered_vars(i)%data, 2), 1]
               call check(nf90_put_var(ncid, registered_vars(i)%varid_avg, &
                                       registered_vars(i)%data, &
                                       start=start, count=count))

            end if

         case ("rst")
            if (registered_vars(i)%to_rst) then
               call check(nf90_put_var(ncid, registered_vars(i)%varid_rst, &
                                       registered_vars(i)%data))
            end if
         end select
      end do

      ! Write time value
      !  if (tag /= "rst" .and. time_varid /= -1) then
      !     time_slice(1) = time_value
      !     call check(nf90_put_var(ncid, time_varid, time_slice, &
      !                             start=[his_time_index], &
      !                             count=[1]))
      !  end if

      ! Increment time index for this file
      !  if (tag /= "rst") then
      !     current_file_time_index = current_file_time_index + 1
      !  end if
   end subroutine write_output

   subroutine check(ierr)
      integer, intent(in) :: ierr
      if (ierr /= nf90_noerr) then
         print *, 'NetCDF error:', trim(nf90_strerror(ierr))
         stop
      end if
   end subroutine check

end module file_manager
