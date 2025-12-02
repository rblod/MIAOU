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
!> - `var_data_ptr` — Pointers to actual data arrays
!> - `io_variable` — Composed type (metadata + data)
!> - `io_var_registry` — Collection of registered variables
!>
!> @author Rachid Benshila
!> @date 2025-04
!===============================================================================
module io_definitions
   use grid_module, only: grid
   use io_constants, only: IO_VARNAME_LEN, IO_LONGNAME_LEN, IO_UNITS_LEN, &
                           IO_INITIAL_ALLOC, IO_GROWTH_FACTOR
   implicit none
   private

   ! Public types
   public :: var_metadata
   public :: var_data_ptr
   public :: io_variable
   public :: io_var_registry

   !---------------------------------------------------------------------------
   !> @brief Variable metadata (identification and description)
   !>
   !> Contains all descriptive information about a variable: name, long name,
   !> units, associated grid, and dimensionality.
   !---------------------------------------------------------------------------
   type :: var_metadata
      character(len=IO_VARNAME_LEN) :: name = ""        !< Short name (e.g., "zeta")
      character(len=IO_LONGNAME_LEN) :: long_name = ""  !< Descriptive name
      character(len=IO_UNITS_LEN) :: units = ""         !< Physical units
      type(grid) :: var_grid                            !< Associated grid
      integer :: ndims = 0                              !< Number of dimensions (0-3)
   end type var_metadata

   !---------------------------------------------------------------------------
   !> @brief Pointers to variable data arrays
   !>
   !> Holds pointers to the actual model data. Only one pointer should be
   !> associated at a time, corresponding to the variable's dimensionality.
   !---------------------------------------------------------------------------
   type :: var_data_ptr
      real, pointer :: scalar => null()       !< Scalar (0D) data
      real, pointer :: d1(:) => null()        !< 1D data array
      real, pointer :: d2(:,:) => null()      !< 2D data array
      real, pointer :: d3(:,:,:) => null()    !< 3D data array
   contains
      !> @brief Check if data pointer is valid for given dimension
      procedure :: is_valid => data_is_valid
      !> @brief Get the shape of the data array
      procedure :: get_shape => data_get_shape
      !> @brief Nullify all pointers
      procedure :: nullify_all => data_nullify_all
   end type var_data_ptr

   !---------------------------------------------------------------------------
   !> @brief Complete I/O variable using composition
   !>
   !> This type represents a model variable that can be output to files.
   !> It contains only the variable's identity and data pointers.
   !> Output configuration (which files, frequencies, operations) is
   !> managed separately by output_file_def in io_file_registry.
   !>
   !> ## Example
   !>
   !> ```fortran
   !> type(io_variable) :: var
   !> var%meta%name = "zeta"
   !> var%meta%units = "m"
   !> var%meta%ndims = 2
   !> var%data%d2 => model_zeta
   !> ```
   !---------------------------------------------------------------------------
   type :: io_variable
      type(var_metadata) :: meta    !< Variable identification
      type(var_data_ptr) :: data    !< Pointers to data
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
   end type io_var_registry

contains

   !===========================================================================
   ! var_data_ptr methods
   !===========================================================================

   !---------------------------------------------------------------------------
   !> @brief Check if data pointer is valid for given dimension
   !---------------------------------------------------------------------------
   function data_is_valid(this, ndims) result(is_valid)
      class(var_data_ptr), intent(in) :: this
      integer, intent(in) :: ndims
      logical :: is_valid

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
   end function data_is_valid

   !---------------------------------------------------------------------------
   !> @brief Get the shape of the data array
   !---------------------------------------------------------------------------
   subroutine data_get_shape(this, ndims, shp)
      class(var_data_ptr), intent(in) :: this
      integer, intent(in) :: ndims
      integer, intent(out) :: shp(:)

      shp = 0
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
   end subroutine data_get_shape

   !---------------------------------------------------------------------------
   !> @brief Nullify all data pointers
   !---------------------------------------------------------------------------
   subroutine data_nullify_all(this)
      class(var_data_ptr), intent(inout) :: this

      nullify(this%scalar)
      nullify(this%d1)
      nullify(this%d2)
      nullify(this%d3)
   end subroutine data_nullify_all

   !===========================================================================
   ! io_var_registry methods
   !===========================================================================

   !---------------------------------------------------------------------------
   !> @brief Add a variable to the registry
   !---------------------------------------------------------------------------
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
         ! Look for empty slot
         do i = 1, size(this%variables)
            if (trim(this%variables(i)%meta%name) == "") then
               this%variables(i) = var
               this%count = max(this%count, i)
               return
            end if
         end do

         ! Expand array
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

   !---------------------------------------------------------------------------
   !> @brief Find a variable by name
   !---------------------------------------------------------------------------
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

   !---------------------------------------------------------------------------
   !> @brief Get the number of registered variables
   !---------------------------------------------------------------------------
   function registry_size(this) result(sz)
      class(io_var_registry), intent(in) :: this
      integer :: sz
      sz = this%count
   end function registry_size

   !---------------------------------------------------------------------------
   !> @brief Get a variable by index (returns a copy)
   !---------------------------------------------------------------------------
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

   !---------------------------------------------------------------------------
   !> @brief Get a pointer to a variable by index
   !---------------------------------------------------------------------------
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

end module io_definitions
