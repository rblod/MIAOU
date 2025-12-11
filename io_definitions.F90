!===============================================================================
!> @file io_definitions.F90
!>
!> @brief Core definitions for the I/O system
!>
!> This module defines the fundamental types for variable representation.
!> Output configuration (files, frequencies, operations) is now handled
!> separately in io_file_registry.
!>
!> ## Key Types
!>
!> - `var_metadata` — Variable identification and description
!> - `var_data_ptr` — Pointers to actual data arrays OR internal buffers
!> - `io_variable` — Composed type (metadata + data)
!> - `io_var_registry` — Collection of registered variables
!>
!> @author Rachid Benshila
!> @date 2025-04
!===============================================================================
module io_definitions
   use grid_module, only: grid
   use io_constants, only: IO_VARNAME_LEN, IO_LONGNAME_LEN, IO_UNITS_LEN, &
                           IO_INITIAL_ALLOC, IO_GROWTH_FACTOR, IO_FILL_UNSET
   implicit none
   private

   ! Public types
   public :: var_metadata
   public :: var_data_ptr
   public :: io_variable
   public :: io_var_registry

   !---------------------------------------------------------------------------
   !> @brief Variable metadata (identification and description)
   !---------------------------------------------------------------------------
   type :: var_metadata
      character(len=IO_VARNAME_LEN) :: name = ""
      character(len=IO_LONGNAME_LEN) :: long_name = ""
      character(len=IO_UNITS_LEN) :: units = ""
      type(grid) :: var_grid
      integer :: ndims = 0
      
      ! CF-compliant attributes
      character(len=64) :: standard_name = ""
      real :: valid_min = -huge(1.0)
      real :: valid_max = huge(1.0)
      real :: fill_value = IO_FILL_UNSET
      character(len=64) :: coordinates = ""
   end type var_metadata

   !---------------------------------------------------------------------------
   !> @brief Pointers to variable data arrays OR internal buffers
   !>
   !> When use_buffer is true, data is stored in internal buffers (buf_*)
   !> and model variables do NOT need 'target' attribute.
   !---------------------------------------------------------------------------
   type :: var_data_ptr
      ! Pointers (used in legacy mode or when pointing to external data)
      real, pointer :: scalar => null()
      real, pointer :: d1(:) => null()
      real, pointer :: d2(:,:) => null()
      real, pointer :: d3(:,:,:) => null()
      
      ! Internal buffers (no target attribute needed on model vars)
      real :: buf_scalar = 0.0
      real, allocatable :: buf_1d(:)
      real, allocatable :: buf_2d(:,:)
      real, allocatable :: buf_3d(:,:,:)
      logical :: use_buffer = .false.
   contains
      procedure :: is_valid => data_is_valid
      procedure :: get_shape => data_get_shape
      procedure :: nullify_all => data_nullify_all
      procedure :: set_0d, set_1d, set_2d, set_3d
      generic :: set => set_0d, set_1d, set_2d, set_3d
      procedure :: get_0d, get_1d, get_2d, get_3d
   end type var_data_ptr

   !---------------------------------------------------------------------------
   !> @brief Complete I/O variable using composition
   !---------------------------------------------------------------------------
   type :: io_variable
      type(var_metadata) :: meta
      type(var_data_ptr) :: data
   end type io_variable

   !---------------------------------------------------------------------------
   !> @brief Registry of variables for output operations
   !---------------------------------------------------------------------------
   type :: io_var_registry
      private
      type(io_variable), allocatable :: variables(:)
      integer :: count = 0
   contains
      procedure :: add => registry_add_variable
      procedure :: find => registry_find_variable
      procedure :: size => registry_size
      procedure :: get => registry_get_variable
      procedure :: get_ptr => registry_get_variable_ptr
      procedure :: clear => registry_clear_variables
   end type io_var_registry

contains

   !===========================================================================
   ! var_data_ptr methods
   !===========================================================================

   function data_is_valid(this, ndims) result(is_valid)
      class(var_data_ptr), intent(in) :: this
      integer, intent(in) :: ndims
      logical :: is_valid

      if (this%use_buffer) then
         select case (ndims)
         case (0)
            is_valid = .true.  ! Scalar buffer always valid
         case (1)
            is_valid = allocated(this%buf_1d)
         case (2)
            is_valid = allocated(this%buf_2d)
         case (3)
            is_valid = allocated(this%buf_3d)
         case default
            is_valid = .false.
         end select
      else
         select case (ndims)
         case (0)
            is_valid = associated(this%scalar)
         case (1)
            is_valid = associated(this%d1)
         case (2)
            is_valid = associated(this%d2)
         case (3)
            is_valid = associated(this%d3)
         case default
            is_valid = .false.
         end select
      end if
   end function data_is_valid

   subroutine data_get_shape(this, ndims, shp)
      class(var_data_ptr), intent(in) :: this
      integer, intent(in) :: ndims
      integer, intent(out) :: shp(:)

      shp = 0
      if (this%use_buffer) then
         select case (ndims)
         case (1)
            if (allocated(this%buf_1d)) shp(1) = size(this%buf_1d)
         case (2)
            if (allocated(this%buf_2d)) then
               shp(1) = size(this%buf_2d, 1)
               shp(2) = size(this%buf_2d, 2)
            end if
         case (3)
            if (allocated(this%buf_3d)) then
               shp(1) = size(this%buf_3d, 1)
               shp(2) = size(this%buf_3d, 2)
               shp(3) = size(this%buf_3d, 3)
            end if
         end select
      else
         select case (ndims)
         case (1)
            if (associated(this%d1)) shp(1) = size(this%d1)
         case (2)
            if (associated(this%d2)) then
               shp(1) = size(this%d2, 1)
               shp(2) = size(this%d2, 2)
            end if
         case (3)
            if (associated(this%d3)) then
               shp(1) = size(this%d3, 1)
               shp(2) = size(this%d3, 2)
               shp(3) = size(this%d3, 3)
            end if
         end select
      end if
   end subroutine data_get_shape

   subroutine data_nullify_all(this)
      class(var_data_ptr), intent(inout) :: this
      nullify(this%scalar)
      nullify(this%d1)
      nullify(this%d2)
      nullify(this%d3)
   end subroutine data_nullify_all

   !---------------------------------------------------------------------------
   ! Set data (copy to internal buffer)
   !---------------------------------------------------------------------------
   subroutine set_0d(this, val)
      class(var_data_ptr), intent(inout) :: this
      real, intent(in) :: val
      this%buf_scalar = val
      this%use_buffer = .true.
   end subroutine set_0d

   subroutine set_1d(this, val)
      class(var_data_ptr), intent(inout) :: this
      real, intent(in) :: val(:)
      if (.not. allocated(this%buf_1d)) then
         allocate(this%buf_1d(size(val)))
      else if (size(this%buf_1d) /= size(val)) then
         deallocate(this%buf_1d)
         allocate(this%buf_1d(size(val)))
      end if
      this%buf_1d(:) = val(:)
      this%use_buffer = .true.
   end subroutine set_1d

   subroutine set_2d(this, val)
      class(var_data_ptr), intent(inout) :: this
      real, intent(in) :: val(:,:)
      if (.not. allocated(this%buf_2d)) then
         allocate(this%buf_2d(size(val,1), size(val,2)))
      else if (size(this%buf_2d,1) /= size(val,1) .or. size(this%buf_2d,2) /= size(val,2)) then
         deallocate(this%buf_2d)
         allocate(this%buf_2d(size(val,1), size(val,2)))
      end if
      this%buf_2d(:,:) = val(:,:)
      this%use_buffer = .true.
   end subroutine set_2d

   subroutine set_3d(this, val)
      class(var_data_ptr), intent(inout) :: this
      real, intent(in) :: val(:,:,:)
      if (.not. allocated(this%buf_3d)) then
         allocate(this%buf_3d(size(val,1), size(val,2), size(val,3)))
      else if (size(this%buf_3d,1) /= size(val,1) .or. &
               size(this%buf_3d,2) /= size(val,2) .or. &
               size(this%buf_3d,3) /= size(val,3)) then
         deallocate(this%buf_3d)
         allocate(this%buf_3d(size(val,1), size(val,2), size(val,3)))
      end if
      this%buf_3d(:,:,:) = val(:,:,:)
      this%use_buffer = .true.
   end subroutine set_3d

   !---------------------------------------------------------------------------
   ! Get data (from buffer or pointer)
   !---------------------------------------------------------------------------
   function get_0d(this) result(val)
      class(var_data_ptr), intent(in) :: this
      real :: val
      if (this%use_buffer) then
         val = this%buf_scalar
      else if (associated(this%scalar)) then
         val = this%scalar
      else
         val = 0.0
      end if
   end function get_0d

   function get_1d(this) result(val)
      class(var_data_ptr), intent(in) :: this
      real, allocatable :: val(:)
      if (this%use_buffer .and. allocated(this%buf_1d)) then
         val = this%buf_1d
      else if (associated(this%d1)) then
         val = this%d1
      end if
   end function get_1d

   function get_2d(this) result(val)
      class(var_data_ptr), intent(in) :: this
      real, allocatable :: val(:,:)
      if (this%use_buffer .and. allocated(this%buf_2d)) then
         val = this%buf_2d
      else if (associated(this%d2)) then
         val = this%d2
      end if
   end function get_2d

   function get_3d(this) result(val)
      class(var_data_ptr), intent(in) :: this
      real, allocatable :: val(:,:,:)
      if (this%use_buffer .and. allocated(this%buf_3d)) then
         val = this%buf_3d
      else if (associated(this%d3)) then
         val = this%d3
      end if
   end function get_3d

   !===========================================================================
   ! io_var_registry methods
   !===========================================================================

   subroutine registry_add_variable(this, var)
      class(io_var_registry), intent(inout) :: this
      type(io_variable), intent(in) :: var
      type(io_variable), allocatable :: temp(:)
      integer :: i, new_size

      if (.not. allocated(this%variables)) then
         allocate(this%variables(IO_INITIAL_ALLOC))
         this%count = 1
         this%variables(1) = var
         do i = 2, IO_INITIAL_ALLOC
            this%variables(i)%meta%name = ""
         end do
      else
         do i = 1, size(this%variables)
            if (trim(this%variables(i)%meta%name) == "") then
               this%variables(i) = var
               this%count = max(this%count, i)
               return
            end if
         end do
         new_size = size(this%variables) + max(5, (size(this%variables) * IO_GROWTH_FACTOR) / 100)
         allocate(temp(new_size))
         temp(1:size(this%variables)) = this%variables
         temp(size(this%variables) + 1) = var
         this%count = size(this%variables) + 1
         do i = this%count + 1, new_size
            temp(i)%meta%name = ""
         end do
         call move_alloc(temp, this%variables)
      end if
   end subroutine registry_add_variable

   function registry_find_variable(this, varname) result(idx)
      class(io_var_registry), intent(in) :: this
      character(len=*), intent(in) :: varname
      integer :: idx, i
      idx = -1
      if (.not. allocated(this%variables)) return
      do i = 1, this%count
         if (trim(this%variables(i)%meta%name) == trim(varname)) then
            idx = i
            return
         end if
      end do
   end function registry_find_variable

   function registry_size(this) result(sz)
      class(io_var_registry), intent(in) :: this
      integer :: sz
      sz = this%count
   end function registry_size

   function registry_get_variable(this, idx) result(var)
      class(io_var_registry), intent(in) :: this
      integer, intent(in) :: idx
      type(io_variable) :: var
      if (allocated(this%variables) .and. idx > 0 .and. idx <= this%count) then
         var = this%variables(idx)
      else
         var%meta%name = ""
      end if
   end function registry_get_variable

   function registry_get_variable_ptr(this, idx) result(var_ptr)
      class(io_var_registry), intent(inout), target :: this
      integer, intent(in) :: idx
      type(io_variable), pointer :: var_ptr
      if (allocated(this%variables) .and. idx > 0 .and. idx <= this%count) then
         var_ptr => this%variables(idx)
      else
         nullify(var_ptr)
      end if
   end function registry_get_variable_ptr

   !---------------------------------------------------------------------------
   !> @brief Clear the variable registry (reset to empty state)
   !>
   !> Deallocates all variables and resets count to zero.
   !> Used for testing and re-initialization.
   !---------------------------------------------------------------------------
   subroutine registry_clear_variables(this)
      class(io_var_registry), intent(inout) :: this
      
      if (allocated(this%variables)) deallocate(this%variables)
      this%count = 0
   end subroutine registry_clear_variables

end module io_definitions
