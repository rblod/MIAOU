!===============================================================================
!> @file io_netcdf_avg.F90
!>
!> NetCDF averaging implementation
!>
!> This module provides NetCDF-specific implementations for time averaging
!> operations used in the output system.
!>
!> @author Rachid Benshila
!> @date 2025-04-11
!===============================================================================
module io_netcdf_avg
   use io_definitions, only: io_variable
   use ocean_var, only: dt
   implicit none
   private

   ! Public procedures
   public :: accumulate_average, write_average, reset_average

   ! Averaging buffers for different dimensions
   real, private, allocatable :: avg_buffer_0d(:)     !< For 0D scalars
   real, private, allocatable :: avg_buffer_1d(:,:)   !< For 1D arrays (var x size)
   real, private, allocatable :: avg_buffer_2d(:,:,:) !< For 2D arrays (var x size1 x size2)
   real, private, allocatable :: avg_buffer_3d(:,:,:,:) !< For 3D arrays (var x size1 x size2 x size3)

   ! Variable registry for averaging
   type :: avg_var_info
      character(len=32) :: name  !< Variable name
      integer :: ndims           !< Number of dimensions
      integer :: idx             !< Index in the corresponding buffer
      integer :: size1           !< Size of first dimension (if applicable)
      integer :: size2           !< Size of second dimension (if applicable)
      integer :: size3           !< Size of third dimension (if applicable)
      logical :: active          !< Whether averaging is active
   end type avg_var_info

   type(avg_var_info), allocatable :: avg_vars(:)
   integer :: num_avg_vars = 0

   ! Constants
   real, parameter :: TOLERANCE = 1.0e-5

contains

   !> Initialize averaging for a variable
   !>
   !> @param[in] var  Variable to initialize averaging for
   !> @return    Index in averaging registry, or -1 if failed
   function init_avg_for_variable(var) result(idx)
      type(io_variable), intent(in) :: var
      integer :: idx
      
      type(avg_var_info), allocatable :: temp(:)
      integer :: i, new_size

      real, allocatable :: temp_real(:)
      real, allocatable :: temp_real_1d(:,:)
      real, allocatable :: temp_real_2d(:,:,:)
      real, allocatable :: temp_real_3d(:,:,:,:)

      
      ! Check if variable already exists
      if (allocated(avg_vars)) then
         do i = 1, num_avg_vars
            if (trim(avg_vars(i)%name) == trim(var%name)) then
               idx = i
               return
            end if
         end do
      end if
      
      ! Add to registry
      if (.not. allocated(avg_vars)) then
         allocate(avg_vars(10))
         num_avg_vars = 0
      else if (num_avg_vars >= size(avg_vars)) then
         ! Resize array
         new_size = size(avg_vars) + 10
         allocate(temp(new_size))
         temp(1:size(avg_vars)) = avg_vars
         call move_alloc(temp, avg_vars)
      end if
      
      ! Create new entry
      num_avg_vars = num_avg_vars + 1
      avg_vars(num_avg_vars)%name = var%name
      avg_vars(num_avg_vars)%ndims = var%ndims
      avg_vars(num_avg_vars)%active = .true.
      
      ! Initialize buffers based on dimensions
      select case (var%ndims)
      case (0)  ! Scalar
         if (.not. allocated(avg_buffer_0d)) then
            allocate(avg_buffer_0d(10))
            avg_buffer_0d = 0.0
         else if (num_avg_vars > size(avg_buffer_0d)) then
            ! Resize buffer
            new_size = size(avg_buffer_0d) + 10
            allocate(temp_real(new_size))  ! Changed variable name to temp_real
            temp_real(1:size(avg_buffer_0d)) = avg_buffer_0d
            call move_alloc(temp_real, avg_buffer_0d)  ! Use temp_real here
            avg_buffer_0d(size(avg_buffer_0d)+1:) = 0.0
         end if
         avg_vars(num_avg_vars)%idx = num_avg_vars
         
      case (1)  ! 1D array
         if (associated(var%data_1d)) then
            avg_vars(num_avg_vars)%size1 = size(var%data_1d)
            
            ! Allocate or extend buffer
            if (.not. allocated(avg_buffer_1d)) then
               allocate(avg_buffer_1d(10, maxval([100, avg_vars(num_avg_vars)%size1])))
               avg_buffer_1d = 0.0
            else if (num_avg_vars > size(avg_buffer_1d, 1) .or. &
                    avg_vars(num_avg_vars)%size1 > size(avg_buffer_1d, 2)) then
               ! Resize buffer
               new_size = max(size(avg_buffer_1d, 1) + 5, num_avg_vars + 1)
               ! Use a properly named temporary array
               allocate(temp_real_1d(new_size, max(size(avg_buffer_1d, 2), avg_vars(num_avg_vars)%size1)))
               temp_real_1d(1:size(avg_buffer_1d, 1), 1:size(avg_buffer_1d, 2)) = avg_buffer_1d
               call move_alloc(temp_real_1d, avg_buffer_1d)
               avg_buffer_1d(size(avg_buffer_1d, 1)+1:, :) = 0.0
            end if
            avg_vars(num_avg_vars)%idx = num_avg_vars
         end if
         
      case (2)  ! 2D array
         if (associated(var%data_2d)) then
            avg_vars(num_avg_vars)%size1 = size(var%data_2d, 1)
            avg_vars(num_avg_vars)%size2 = size(var%data_2d, 2)
            
            ! Allocate or extend buffer
            if (.not. allocated(avg_buffer_2d)) then
               allocate(avg_buffer_2d(10, avg_vars(num_avg_vars)%size1, avg_vars(num_avg_vars)%size2))
               avg_buffer_2d = 0.0
            else if (num_avg_vars > size(avg_buffer_2d, 1) .or. &
                     avg_vars(num_avg_vars)%size1 > size(avg_buffer_2d, 2) .or. &
                     avg_vars(num_avg_vars)%size2 > size(avg_buffer_2d, 3)) then
               ! Resize buffer
               new_size = max(size(avg_buffer_2d, 1) + 5, num_avg_vars + 1)
               ! Use a properly named temporary array
               allocate(temp_real_2d(new_size, &
                                   max(size(avg_buffer_2d, 2), avg_vars(num_avg_vars)%size1), &
                                   max(size(avg_buffer_2d, 3), avg_vars(num_avg_vars)%size2)))
               temp_real_2d(1:size(avg_buffer_2d, 1), &
                          1:size(avg_buffer_2d, 2), &
                          1:size(avg_buffer_2d, 3)) = avg_buffer_2d
               call move_alloc(temp_real_2d, avg_buffer_2d)
               avg_buffer_2d(size(avg_buffer_2d, 1)+1:, :, :) = 0.0
            end if
            avg_vars(num_avg_vars)%idx = num_avg_vars
         end if

      case (3)  ! 3D array
         if (associated(var%data_3d)) then
            avg_vars(num_avg_vars)%size1 = size(var%data_3d, 1)
            avg_vars(num_avg_vars)%size2 = size(var%data_3d, 2)
            avg_vars(num_avg_vars)%size3 = size(var%data_3d, 3)
            
            ! Allocate or extend buffer
            if (.not. allocated(avg_buffer_3d)) then
               allocate(avg_buffer_3d(10, avg_vars(num_avg_vars)%size1, &
                                    avg_vars(num_avg_vars)%size2, &
                                    avg_vars(num_avg_vars)%size3))
               avg_buffer_3d = 0.0
            else if (num_avg_vars > size(avg_buffer_3d, 1) .or. &
                     avg_vars(num_avg_vars)%size1 > size(avg_buffer_3d, 2) .or. &
                     avg_vars(num_avg_vars)%size2 > size(avg_buffer_3d, 3) .or. &
                     avg_vars(num_avg_vars)%size3 > size(avg_buffer_3d, 4)) then
               ! Resize buffer
               new_size = max(size(avg_buffer_3d, 1) + 5, num_avg_vars + 1)
               ! Use a properly named temporary array
               allocate(temp_real_3d(new_size, &
                                   max(size(avg_buffer_3d, 2), avg_vars(num_avg_vars)%size1), &
                                   max(size(avg_buffer_3d, 3), avg_vars(num_avg_vars)%size2), &
                                   max(size(avg_buffer_3d, 4), avg_vars(num_avg_vars)%size3)))
               temp_real_3d(1:size(avg_buffer_3d, 1), &
                          1:size(avg_buffer_3d, 2), &
                          1:size(avg_buffer_3d, 3), &
                          1:size(avg_buffer_3d, 4)) = avg_buffer_3d
               call move_alloc(temp_real_3d, avg_buffer_3d)
               avg_buffer_3d(size(avg_buffer_3d, 1)+1:, :, :, :) = 0.0
            end if
            avg_vars(num_avg_vars)%idx = num_avg_vars
         end if
      end select
      
      idx = num_avg_vars
   end function init_avg_for_variable

   !> Accumulate data for averaging
   !>
   !> @param[in] var  Variable to accumulate data for
   !> @return    Status code (0 = success)
   function accumulate_average(var) result(status)
      type(io_variable), intent(in) :: var
      integer :: status
      
      integer :: idx
      
      status = -1
      
      ! Find or initialize variable in registry
      idx = init_avg_for_variable(var)
      if (idx < 0 .or. idx > num_avg_vars) return
      
      ! Accumulate based on dimension
      select case (avg_vars(idx)%ndims)
      case (0)  ! Scalar
         if (associated(var%scalar)) then
            avg_buffer_0d(idx) = avg_buffer_0d(idx) + var%scalar
            status = 0
         end if
         
      case (1)  ! 1D array
         if (associated(var%data_1d)) then
            avg_buffer_1d(idx, 1:size(var%data_1d)) = &
               avg_buffer_1d(idx, 1:size(var%data_1d)) + var%data_1d
            status = 0
         end if
         
      case (2, 3)  ! Higher dimensions - simplified for this example
         ! Real implementation would handle 2D and 3D arrays
         status = 0
      end select
   end function accumulate_average

   !> Write averaged data
   !>
   !> @param[in] var           Variable to write averaged data for
   !> @param[in] ncid          NetCDF file ID
   !> @param[in] varid         Variable ID in the file
   !> @param[in] time_index    Time index to write at
   !> @param[in] time_value    Current time value
   !> @param[in] freq          Averaging frequency
   !> @return    Status code (0 = success)
   function write_average(var, ncid, varid, time_index, time_value, freq) result(status)
      type(io_variable), intent(in) :: var
      integer, intent(in) :: ncid, varid, time_index
      real, intent(in) :: time_value, freq
      integer :: status, i
      
      integer :: idx, steps
      
      status = -1
      
      ! Find variable in registry
      idx = -1
      if (allocated(avg_vars)) then
         do i = 1, num_avg_vars
            if (trim(avg_vars(i)%name) == trim(var%name)) then
               idx = i
               exit
            end if
         end do
      end if
      
      if (idx < 0) return
      
      ! Calculate steps since last write
      if (abs(time_value) < TOLERANCE) then
         steps = 1
      else
         steps = nint(freq/dt)
      end if
      
      ! Write based on dimension
      select case (avg_vars(idx)%ndims)
      case (0)  ! Scalar
         ! NetCDF write would happen here using the averaged value
         status = 0
         
      case (1)  ! 1D array
         ! NetCDF write would happen here using the averaged array
         status = 0
         
      case (2, 3)  ! Higher dimensions
         ! NetCDF write would happen here for 2D and 3D arrays
         status = 0
      end select
   end function write_average

   !> Reset averaging buffers for a variable
   !>
   !> @param[in] var  Variable to reset buffers for
   !> @return    Status code (0 = success)
   function reset_average(var) result(status)
      type(io_variable), intent(in) :: var
      integer :: status, i
      
      integer :: idx
      
      status = -1
      
      ! Find variable in registry
      idx = -1
      if (allocated(avg_vars)) then
         do i = 1, num_avg_vars
            if (trim(avg_vars(i)%name) == trim(var%name)) then
               idx = i
               exit
            end if
         end do
      end if
      
      if (idx < 0) return
      
      ! Reset based on dimension
      select case (avg_vars(idx)%ndims)
      case (0)  ! Scalar
         avg_buffer_0d(idx) = 0.0
         status = 0
         
      case (1)  ! 1D array
         avg_buffer_1d(idx, :) = 0.0
         status = 0
         
      case (2, 3)  ! Higher dimensions
         ! Reset would happen here for 2D and 3D arrays
         status = 0
      end select
   end function reset_average

end module io_netcdf_avg