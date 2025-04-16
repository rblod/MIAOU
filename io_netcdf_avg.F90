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
      use netcdf_backend, only: nc_define_variable, nc_write_variable

   use ocean_var, only: dt
   implicit none
   private

   ! Public procedures
   public :: accumulate_average, write_average, reset_average
   public :: init_avg_buffers, accumulate_avg, write_variable_avg, reset_avg

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
      
   case (2)  ! 2D array
      if (associated(var%data_2d)) then
         ! Vérifier les dimensions
         if (avg_vars(idx)%size1 == size(var%data_2d, 1) .and. &
             avg_vars(idx)%size2 == size(var%data_2d, 2)) then
            
            avg_buffer_2d(idx, 1:avg_vars(idx)%size1, 1:avg_vars(idx)%size2) = &
               avg_buffer_2d(idx, 1:avg_vars(idx)%size1, 1:avg_vars(idx)%size2) + var%data_2d
            
            status = 0
         else
            print *, "Warning: Dimension mismatch for variable ", trim(var%name), &
                     " in accumulate_average"
         end if
      end if
      
   case (3)  ! 3D array
      if (associated(var%data_3d)) then
         ! Vérifier les dimensions
         if (avg_vars(idx)%size1 == size(var%data_3d, 1) .and. &
             avg_vars(idx)%size2 == size(var%data_3d, 2) .and. &
             avg_vars(idx)%size3 == size(var%data_3d, 3)) then
            
            avg_buffer_3d(idx, 1:avg_vars(idx)%size1, 1:avg_vars(idx)%size2, 1:avg_vars(idx)%size3) = &
               avg_buffer_3d(idx, 1:avg_vars(idx)%size1, 1:avg_vars(idx)%size2, 1:avg_vars(idx)%size3) + &
               var%data_3d
            
            status = 0
         else
            print *, "Warning: Dimension mismatch for variable ", trim(var%name), &
                     " in accumulate_average"
         end if
      end if
   end select
end function accumulate_average

function write_average(var, ncid, varid, time_index, time_value, freq) result(status)
   type(io_variable), intent(in) :: var
   integer, intent(in) :: ncid, varid, time_index
   real, intent(in) :: time_value, freq
   integer :: status, i
   
   integer :: idx, steps
   real :: avg_scalar
   real, allocatable :: avg_data_1d(:)
   real, allocatable :: avg_data_2d(:,:)
   real, allocatable :: avg_data_3d(:,:,:)
   


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
  print *, "Debug write_average for: ", trim(var%name)
print *, "  Time=", time_value, " Steps=", steps
if (idx > 0 .and. avg_vars(idx)%ndims == 0) then
    print *, "  Buffer max value=", (avg_buffer_0d(idx))
    print *, "  Buffer min value=", (avg_buffer_0d(idx))
end if

   
   ! Calculate steps since last write
   if (abs(time_value) < TOLERANCE) then
      steps = 1
   else
      steps = nint(freq/dt)
   end if
   
   print *, "Writing average for variable ", trim(var%name), &
            " at time ", time_value, ", steps = ", steps, 'freq = ',freq, var%scalar
   
   ! Write based on dimension
   select case (avg_vars(idx)%ndims)
   case (0)  ! Scalar
      ! Calculate average by dividing by number of steps
   if (associated(var%scalar)) then
      avg_buffer_0d(idx) = avg_buffer_0d(idx) / steps
      call nc_write_variable(ncid, varid, avg_buffer_0d(idx), time_index)
      status = 0
   end if
      
   case (1)  ! 1D array
      ! Calculate average
      allocate(avg_data_1d(avg_vars(idx)%size1))
      avg_data_1d = avg_buffer_1d(idx, 1:avg_vars(idx)%size1) / steps
      
      ! Write to NetCDF
      call nc_write_variable(ncid, varid, avg_data_1d, time_index)  ! Utilise nc_write_1d
      deallocate(avg_data_1d)
      status = 0
      
   case (2)  ! 2D array
      ! Calculate average
      allocate(avg_data_2d(avg_vars(idx)%size1, avg_vars(idx)%size2))
      avg_data_2d = avg_buffer_2d(idx, 1:avg_vars(idx)%size1, 1:avg_vars(idx)%size2) / steps
      
      ! Write to NetCDF
      call nc_write_variable(ncid, varid, avg_data_2d, time_index)  ! Utilise nc_write_2d
      deallocate(avg_data_2d)
      status = 0
      
   case (3)  ! 3D array
      ! Calculate average
      allocate(avg_data_3d(avg_vars(idx)%size1, avg_vars(idx)%size2, avg_vars(idx)%size3))
      avg_data_3d = avg_buffer_3d(idx, 1:avg_vars(idx)%size1, 1:avg_vars(idx)%size2, 1:avg_vars(idx)%size3) / steps
      
      ! Write to NetCDF
      call nc_write_variable(ncid, varid, avg_data_3d, time_index)  ! Utilise nc_write_3d
      deallocate(avg_data_3d)
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
      
   case (2)  ! 2D array
      avg_buffer_2d(idx, :, :) = 0.0
      status = 0
      
   case (3)  ! 3D array
      avg_buffer_3d(idx, :, :, :) = 0.0
      status = 0
   end select
end function reset_average


! Dans io_netcdf_avg.F90 ou un nouveau module
subroutine init_avg_buffers(var)
   type(io_variable), intent(inout) :: var
   
   if (var%avg_initialized) return  ! Déjà initialisé
   
   select case (var%ndims)
   case (0)  ! Scalaire
      var%scalar_avg = 0.0
   case (1)  ! 1D
      if (associated(var%data_1d)) then
         if (allocated(var%data_avg_1d)) deallocate(var%data_avg_1d)
         allocate(var%data_avg_1d(size(var%data_1d)))
         var%data_avg_1d = 0.0
      end if
   case (2)  ! 2D
      if (associated(var%data_2d)) then
         if (allocated(var%data_avg_2d)) deallocate(var%data_avg_2d)
         allocate(var%data_avg_2d(size(var%data_2d,1), size(var%data_2d,2)))
         var%data_avg_2d = 0.0
      end if
   case (3)  ! 3D
      if (associated(var%data_3d)) then
         if (allocated(var%data_avg_3d)) deallocate(var%data_avg_3d)
         allocate(var%data_avg_3d(size(var%data_3d,1), size(var%data_3d,2), size(var%data_3d,3)))
         var%data_avg_3d = 0.0
      end if
   end select
   
   var%avg_count = 0
   var%avg_initialized = .true.
end subroutine init_avg_buffers

subroutine accumulate_avg(var)
   type(io_variable), intent(inout) :: var
   
   if (.not. var%to_avg) return  ! Ne pas accumuler si non requis
   
   ! Initialiser les buffers si nécessaire
   if (.not. var%avg_initialized) call init_avg_buffers(var)
   
   ! Accumuler selon les dimensions
   select case (var%ndims)
   case (0)  ! Scalaire
      if (associated(var%scalar)) then
         var%scalar_avg = var%scalar_avg + var%scalar
      end if
   case (1)  ! 1D
      if (associated(var%data_1d) .and. allocated(var%data_avg_1d)) then
         var%data_avg_1d = var%data_avg_1d + var%data_1d
      end if
   case (2)  ! 2D
      if (associated(var%data_2d) .and. allocated(var%data_avg_2d)) then
         var%data_avg_2d = var%data_avg_2d + var%data_2d
      end if
   case (3)  ! 3D
      if (associated(var%data_3d) .and. allocated(var%data_avg_3d)) then
         var%data_avg_3d = var%data_avg_3d + var%data_3d
      end if
   end select
   
   var%avg_count = var%avg_count + 1
end subroutine accumulate_avg

function write_variable_avg(var, ncid, varid, time_index) result(status)
   type(io_variable), intent(inout) :: var
   integer, intent(in) :: ncid, varid, time_index
   integer :: status
   
   status = -1
   if (.not. var%avg_initialized .or. var%avg_count == 0) return
   
   ! Calculer la moyenne et écrire
   select case (var%ndims)
   case (0)  ! Scalaire
      call nc_write_variable(ncid, varid, var%scalar_avg / var%avg_count, time_index)
   case (1)  ! 1D
      if (allocated(var%data_avg_1d)) then
         call nc_write_variable(ncid, varid, var%data_avg_1d / var%avg_count, time_index)
      end if
   case (2)  ! 2D
      if (allocated(var%data_avg_2d)) then
         call nc_write_variable(ncid, varid, var%data_avg_2d / var%avg_count, time_index)
      end if
   case (3)  ! 3D
      if (allocated(var%data_avg_3d)) then
         call nc_write_variable(ncid, varid, var%data_avg_3d / var%avg_count, time_index)
      end if
   end select
   
   status = 0
end function write_variable_avg

subroutine reset_avg(var)
   type(io_variable), intent(inout) :: var
   
   if (.not. var%avg_initialized) return
   
   select case (var%ndims)
   case (0)  ! Scalaire
      var%scalar_avg = 0.0
   case (1)  ! 1D
      if (allocated(var%data_avg_1d)) var%data_avg_1d = 0.0
   case (2)  ! 2D
      if (allocated(var%data_avg_2d)) var%data_avg_2d = 0.0
   case (3)  ! 3D
      if (allocated(var%data_avg_3d)) var%data_avg_3d = 0.0
   end select
   
   var%avg_count = 0
end subroutine reset_avg
end module io_netcdf_avg