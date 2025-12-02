!===============================================================================
!> @file io_definitions.F90
!>
!> Core definitions for the I/O system
!>
!> This module defines the fundamental types and interfaces for the I/O system.
!> It establishes the common structures used throughout the output system,
!> independent of any specific backend implementation.
!>
!> @author Rachid Benshila
!> @date 2025-04-11
!===============================================================================
module io_definitions
   use grid_module, only: grid
   use io_constants, only: IO_VARNAME_LEN, IO_LONGNAME_LEN, IO_UNITS_LEN, &
                           IO_PREFIX_LEN, IO_PATH_LEN, IO_FILETYPE_LEN, &
                           IO_FREQ_DISABLED, IO_INITIAL_ALLOC, IO_GROWTH_FACTOR
   implicit none
   private

   ! Public types and interfaces
   public :: io_variable, file_descriptor
   public :: io_var_registry

   !> @type io_variable
   !> Fundamental variable type for I/O representation
   !>
   !> This type represents a model variable that can be output to files.
   !> It contains metadata and configuration for output operations,
   !> independent of the specific storage format.
   type :: io_variable
      ! Basic metadata
      character(len=IO_VARNAME_LEN) :: name = ""        !< short name
      character(len=IO_LONGNAME_LEN) :: long_name = ""  !< long name
      character(len=IO_UNITS_LEN) :: units = ""         !< unit
      type(grid) :: var_grid                            !< associated grid
      integer :: ndims = 0                              !< number of dimensions

      ! Output flags
      logical :: to_his = .false.     !< write to history
      logical :: to_avg = .false.     !< write to average  
      logical :: to_rst = .false.     ! write to restart 

      ! Data pointers for different dimensionality
      real, pointer :: scalar => null()             !< scalar data
      real, pointer :: data_1d(:) => null()         !< 1D data 
      real, pointer :: data_2d(:, :) => null()      !< 2D data
      real, pointer :: data_3d(:, :, :) => null()   !< 3D data

      ! Output frequency settings
      real :: freq_his = IO_FREQ_DISABLED           !< history frequency
      real :: freq_avg = IO_FREQ_DISABLED           !< average frequency
      real :: freq_rst = IO_FREQ_DISABLED           !< restart frequency
      character(len=IO_PREFIX_LEN) :: file_prefix = ""  !< file prefix

      ! Fields for average accumulation
      real :: scalar_avg = 0.0                      !< average buffer for scalar (0D) values
      real, allocatable :: data_avg_1d(:)           !< average buffer for 1D arrays
      real, allocatable :: data_avg_2d(:, :)        !< average buffer for 2D arrays
      real, allocatable :: data_avg_3d(:, :, :)     !< average buffer for 3D arrays
      integer :: avg_count = 0                      !< Counter for number of accumulations
      logical :: avg_initialized = .false.          !< Flag indicating if buffers are initialized
   end type io_variable

   !> @type file_descriptor
   !> Descriptor for an output file
   !>
   !> This type provides a complete description of an output file,
   !> containing information about its type, frequency, and backend-specific
   !> metadata. All file state is centralized here to avoid duplication.
   type :: file_descriptor
      ! Core file information
      character(len=IO_PATH_LEN) :: filename = ""   !< Complete filename
      character(len=IO_FILETYPE_LEN) :: type = ""   !< "his" (history), "avg" (average), or "rst" (restart)
      real :: freq = IO_FREQ_DISABLED               !< Write frequency (seconds)
      integer :: time_index = 1                     !< Current time write index

      ! Backend-specific identifiers (NetCDF)
      integer :: backend_id = -1            !< Backend file ID (e.g., NetCDF ncid)
      integer :: time_dimid = -1            !< Time dimension ID in the file
      integer :: time_varid = -1            !< Time variable ID in the file
   contains
      !> Increment time index after writing
      procedure :: increment_time => file_increment_time
      !> Check if file is valid/open
      procedure :: is_open => file_is_open
   end type file_descriptor

   !> @type io_var_registry
   !> Registry of variables for output operations
   !>
   !> This type manages the collection of variables registered for output,
   !> providing methods to add, find, and manipulate variables.
   type :: io_var_registry
      private
      type(io_variable), allocatable :: variables(:)  !< Array of registered variables
      integer :: count = 0                            !< Number of registered variables

   contains
      !> Add a variable to the registry
      procedure :: add => registry_add_variable

      !> Find a variable by name
      procedure :: find => registry_find_variable

      !> Get the number of registered variables
      procedure :: size => registry_size

      !> Get a variable by index (returns a copy)
      procedure :: get => registry_get_variable
      
      !> Get a pointer to a variable by index (for modification)
      procedure :: get_ptr => registry_get_variable_ptr
      
      !> Update a variable in the registry
      procedure :: update => registry_update_variable
   end type io_var_registry

contains

   !---------------------------------------------------------------------------
   ! file_descriptor methods
   !---------------------------------------------------------------------------

   !> Increment the time index for a file
   subroutine file_increment_time(this)
      class(file_descriptor), intent(inout) :: this
      this%time_index = this%time_index + 1
   end subroutine file_increment_time

   !> Check if a file is open/valid
   function file_is_open(this) result(is_open)
      class(file_descriptor), intent(in) :: this
      logical :: is_open
      is_open = (this%backend_id > 0)
   end function file_is_open

   !---------------------------------------------------------------------------
   ! io_var_registry methods
   !---------------------------------------------------------------------------

   !> Add a variable to the registry
   !>
   !> @param[inout] this  The registry to modify
   !> @param[in]    var   The variable to add
   subroutine registry_add_variable(this, var)
      class(io_var_registry), intent(inout) :: this
      type(io_variable), intent(in) :: var
      type(io_variable), allocatable :: temp(:)
      integer :: i, new_size

      if (.not. allocated(this%variables)) then
         ! Initial allocation with extra space
         allocate (this%variables(IO_INITIAL_ALLOC))
         this%count = 1
         this%variables(1) = var

         ! Mark other elements as unused
         do i = 2, IO_INITIAL_ALLOC
            this%variables(i)%name = ""
         end do
      else
         ! Look for an empty slot first
         do i = 1, size(this%variables)
            if (trim(this%variables(i)%name) == "") then
               this%variables(i) = var
               this%count = max(this%count, i)
               return
            end if
         end do

         ! No empty slots, need to expand
         new_size = size(this%variables) + max(5, (size(this%variables) * IO_GROWTH_FACTOR) / 100)
         allocate (temp(new_size))

         ! Copy existing data
         temp(1:size(this%variables)) = this%variables

         ! Add new variable
         temp(size(this%variables) + 1) = var
         this%count = size(this%variables) + 1

         ! Mark new slots as empty
         do i = this%count + 1, new_size
            temp(i)%name = ""
         end do

         ! Replace old array with new one
         call move_alloc(temp, this%variables)
      end if
   end subroutine registry_add_variable

   !> Find a variable by name
   !>
   !> @param[in]  this     The registry to search
   !> @param[in]  varname  Name of the variable to find
   !> @return     Index of the variable, or -1 if not found
   function registry_find_variable(this, varname) result(idx)
      class(io_var_registry), intent(in) :: this
      character(len=*), intent(in) :: varname
      integer :: idx, i

      idx = -1
      if (.not. allocated(this%variables)) return

      do i = 1, this%count
         if (trim(this%variables(i)%name) == trim(varname)) then
            idx = i
            return
         end if
      end do
   end function registry_find_variable

   !> Get the number of registered variables
   !>
   !> @param[in]  this  The registry to query
   !> @return     Number of variables in the registry
   function registry_size(this) result(sz)
      class(io_var_registry), intent(in) :: this
      integer :: sz

      sz = this%count
   end function registry_size

   !> Get a variable by index (returns a copy)
   !>
   !> @param[in]  this  The registry to query
   !> @param[in]  idx   Index of the variable to retrieve
   !> @return     The requested variable (or uninitialized if invalid index)
   function registry_get_variable(this, idx) result(var)
      class(io_var_registry), intent(in) :: this
      integer, intent(in) :: idx
      type(io_variable) :: var

      if (allocated(this%variables) .and. idx > 0 .and. idx <= this%count) then
         var = this%variables(idx)
      else
         var%name = ""  ! Mark as invalid
      end if
   end function registry_get_variable

   !> Get a pointer to a variable by index (for modification)
   !>
   !> @param[inout] this  The registry to query
   !> @param[in]    idx   Index of the variable to retrieve
   !> @return       Pointer to the variable, or null if invalid index
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

   !> Update a variable in the registry
   !>
   !> @param[inout] this  The registry to modify
   !> @param[in]    idx   Index of the variable to update
   !> @param[in]    var   New variable data
   subroutine registry_update_variable(this, idx, var)
      class(io_var_registry), intent(inout) :: this
      integer, intent(in) :: idx
      type(io_variable), intent(in) :: var

      if (allocated(this%variables) .and. idx > 0 .and. idx <= this%count) then
         this%variables(idx) = var
      end if
   end subroutine registry_update_variable

end module io_definitions
