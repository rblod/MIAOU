!===============================================================================
!> @file io_definitions.F90
!>
!> @brief Core definitions for the I/O system
!>
!> This module defines the fundamental types and interfaces for the I/O system.
!> It establishes the common structures used throughout the output system,
!> independent of any specific backend implementation.
!>
!> ## Key Types
!>
!> - `output_stream` — Configuration for a single output stream (his/avg/rst)
!> - `io_variable` — Variable metadata, data pointers, and output configuration
!> - `file_descriptor` — Output file state and backend-specific metadata
!> - `io_var_registry` — Collection of registered variables
!>
!> @author Rachid Benshila
!> @date 2025-04
!===============================================================================
module io_definitions
   use grid_module, only: grid
   use io_constants, only: IO_VARNAME_LEN, IO_LONGNAME_LEN, IO_UNITS_LEN, &
                           IO_PREFIX_LEN, IO_PATH_LEN, IO_FILETYPE_LEN, &
                           IO_FREQ_DISABLED, IO_INITIAL_ALLOC, IO_GROWTH_FACTOR, &
                           IO_TIME_TOLERANCE, IO_NUM_FILE_TYPES, &
                           IO_TYPE_HIS, IO_TYPE_AVG, IO_TYPE_RST
   implicit none
   private

   ! Public types and interfaces
   public :: output_stream
   public :: io_variable, file_descriptor
   public :: io_var_registry

   ! Public constants for stream indices
   integer, public, parameter :: STREAM_HIS = 1  !< Index for history stream
   integer, public, parameter :: STREAM_AVG = 2  !< Index for average stream
   integer, public, parameter :: STREAM_RST = 3  !< Index for restart stream

   !---------------------------------------------------------------------------
   !> @brief Configuration for a single output stream
   !>
   !> An output stream represents one type of output (history, average, restart).
   !> Each variable can have multiple streams with independent configurations.
   !>
   !> ## Example
   !>
   !> ```fortran
   !> type(output_stream) :: stream
   !> stream = output_stream("his", .true., 3600.0, "ocean")
   !> if (stream%should_write(current_time)) then
   !>    ! write data
   !> end if
   !> ```
   !---------------------------------------------------------------------------
   type :: output_stream
      character(len=IO_FILETYPE_LEN) :: stream_type = ""  !< Stream type: "his", "avg", "rst"
      logical :: enabled = .false.                        !< Write to this stream?
      real :: frequency = IO_FREQ_DISABLED                !< Output frequency (seconds)
      character(len=IO_PREFIX_LEN) :: prefix = ""         !< File prefix (empty = use global)
   contains
      !> @brief Check if output should occur at given time
      procedure :: should_write => stream_should_write
      !> @brief Check if this is an averaging stream
      procedure :: is_averaging => stream_is_averaging
      !> @brief Check if stream is enabled with valid frequency
      procedure :: is_active => stream_is_active
   end type output_stream

   !---------------------------------------------------------------------------
   !> @brief Fundamental variable type for I/O representation
   !>
   !> This type represents a model variable that can be output to files.
   !> It contains metadata, data pointers, output stream configuration,
   !> and averaging buffers.
   !>
   !> ## Output Streams
   !>
   !> Instead of separate to_his/freq_his/to_avg/freq_avg/to_rst/freq_rst fields,
   !> all output configuration is stored in the `streams` array:
   !>
   !> - `streams(STREAM_HIS)` — History output configuration
   !> - `streams(STREAM_AVG)` — Average output configuration
   !> - `streams(STREAM_RST)` — Restart output configuration
   !>
   !> This design allows easy iteration and extension to new stream types.
   !---------------------------------------------------------------------------
   type :: io_variable
      ! Basic metadata
      character(len=IO_VARNAME_LEN) :: name = ""        !< Short name
      character(len=IO_LONGNAME_LEN) :: long_name = ""  !< Long descriptive name
      character(len=IO_UNITS_LEN) :: units = ""         !< Physical units
      type(grid) :: var_grid                            !< Associated grid
      integer :: ndims = 0                              !< Number of dimensions (0-3)

      ! Data pointers for different dimensionality (mutually exclusive)
      real, pointer :: scalar => null()             !< Scalar (0D) data
      real, pointer :: data_1d(:) => null()         !< 1D data array
      real, pointer :: data_2d(:, :) => null()      !< 2D data array
      real, pointer :: data_3d(:, :, :) => null()   !< 3D data array

      ! Output stream configuration (replaces to_his/freq_his/to_avg/freq_avg/to_rst/freq_rst)
      type(output_stream) :: streams(IO_NUM_FILE_TYPES)  !< Output streams (his, avg, rst)

      ! Fields for average accumulation
      real :: scalar_avg = 0.0                      !< Average buffer for scalar (0D) values
      real, allocatable :: data_avg_1d(:)           !< Average buffer for 1D arrays
      real, allocatable :: data_avg_2d(:, :)        !< Average buffer for 2D arrays
      real, allocatable :: data_avg_3d(:, :, :)     !< Average buffer for 3D arrays
      integer :: avg_count = 0                      !< Counter for number of accumulations
      logical :: avg_initialized = .false.          !< Flag indicating if buffers are initialized
   contains
      !> @brief Check if variable should be written to any stream
      procedure :: has_output => variable_has_output
      !> @brief Check if variable requires averaging
      procedure :: needs_averaging => variable_needs_averaging
      !> @brief Get the file prefix (from stream or default)
      procedure :: get_prefix => variable_get_prefix
   end type io_variable

   !---------------------------------------------------------------------------
   !> @brief Descriptor for an output file
   !>
   !> This type provides a complete description of an output file,
   !> containing information about its type, frequency, and backend-specific
   !> metadata. All file state is centralized here to avoid duplication.
   !---------------------------------------------------------------------------
   type :: file_descriptor
      ! Core file information
      character(len=IO_PATH_LEN) :: filename = ""   !< Complete filename
      character(len=IO_FILETYPE_LEN) :: type = ""   !< File type: "his", "avg", "rst"
      real :: freq = IO_FREQ_DISABLED               !< Write frequency (seconds)
      integer :: time_index = 1                     !< Current time write index

      ! Backend-specific identifiers (NetCDF)
      integer :: backend_id = -1            !< Backend file ID (e.g., NetCDF ncid)
      integer :: time_dimid = -1            !< Time dimension ID in the file
      integer :: time_varid = -1            !< Time variable ID in the file
   contains
      !> @brief Increment time index after writing
      procedure :: increment_time => file_increment_time
      !> @brief Check if file is valid/open
      procedure :: is_open => file_is_open
   end type file_descriptor

   !---------------------------------------------------------------------------
   !> @brief Registry of variables for output operations
   !>
   !> This type manages the collection of variables registered for output,
   !> providing methods to add, find, and manipulate variables.
   !---------------------------------------------------------------------------
   type :: io_var_registry
      private
      type(io_variable), allocatable :: variables(:)  !< Array of registered variables
      integer :: count = 0                            !< Number of registered variables
   contains
      !> @brief Add a variable to the registry
      procedure :: add => registry_add_variable
      !> @brief Find a variable by name
      procedure :: find => registry_find_variable
      !> @brief Get the number of registered variables
      procedure :: size => registry_size
      !> @brief Get a variable by index (returns a copy)
      procedure :: get => registry_get_variable
      !> @brief Get a pointer to a variable by index (for modification)
      procedure :: get_ptr => registry_get_variable_ptr
      !> @brief Update a variable in the registry
      procedure :: update => registry_update_variable
   end type io_var_registry

contains

   !---------------------------------------------------------------------------
   ! output_stream methods
   !---------------------------------------------------------------------------

   !---------------------------------------------------------------------------
   !> @brief Check if output should occur at the given time
   !>
   !> Returns true if this stream is enabled and the current time aligns
   !> with the output frequency.
   !>
   !> @param[in] this          The output stream
   !> @param[in] current_time  Current simulation time (seconds)
   !> @return    True if output should occur
   !---------------------------------------------------------------------------
   function stream_should_write(this, current_time) result(should_write)
      class(output_stream), intent(in) :: this
      real, intent(in) :: current_time
      logical :: should_write

      should_write = .false.
      
      ! Must be enabled with valid frequency
      if (.not. this%enabled) return
      if (this%frequency <= 0.0) return

      ! Check if time aligns with frequency
      if (abs(current_time) < IO_TIME_TOLERANCE) then
         ! Always write at t=0
         should_write = .true.
      else
         should_write = (abs(mod(current_time, this%frequency)) < IO_TIME_TOLERANCE)
      end if
   end function stream_should_write

   !---------------------------------------------------------------------------
   !> @brief Check if this is an averaging stream
   !>
   !> @param[in] this  The output stream
   !> @return    True if stream_type is "avg"
   !---------------------------------------------------------------------------
   function stream_is_averaging(this) result(is_avg)
      class(output_stream), intent(in) :: this
      logical :: is_avg

      is_avg = (trim(this%stream_type) == IO_TYPE_AVG)
   end function stream_is_averaging

   !---------------------------------------------------------------------------
   !> @brief Check if stream is enabled with valid frequency
   !>
   !> @param[in] this  The output stream
   !> @return    True if enabled and frequency > 0
   !---------------------------------------------------------------------------
   function stream_is_active(this) result(is_active)
      class(output_stream), intent(in) :: this
      logical :: is_active

      is_active = (this%enabled .and. this%frequency > 0.0)
   end function stream_is_active

   !---------------------------------------------------------------------------
   ! io_variable methods
   !---------------------------------------------------------------------------

   !---------------------------------------------------------------------------
   !> @brief Check if variable has any active output stream
   !>
   !> @param[in] this  The variable
   !> @return    True if at least one stream is active
   !---------------------------------------------------------------------------
   function variable_has_output(this) result(has_output)
      class(io_variable), intent(in) :: this
      logical :: has_output
      integer :: i

      has_output = .false.
      do i = 1, IO_NUM_FILE_TYPES
         if (this%streams(i)%is_active()) then
            has_output = .true.
            return
         end if
      end do
   end function variable_has_output

   !---------------------------------------------------------------------------
   !> @brief Check if variable requires averaging
   !>
   !> @param[in] this  The variable
   !> @return    True if the averaging stream is active
   !---------------------------------------------------------------------------
   function variable_needs_averaging(this) result(needs_avg)
      class(io_variable), intent(in) :: this
      logical :: needs_avg

      needs_avg = this%streams(STREAM_AVG)%is_active()
   end function variable_needs_averaging

   !---------------------------------------------------------------------------
   !> @brief Get the file prefix for a given stream
   !>
   !> Returns the stream-specific prefix if set, empty string otherwise.
   !> The caller should fall back to global prefix if empty.
   !>
   !> @param[in] this          The variable
   !> @param[in] stream_index  Stream index (STREAM_HIS, STREAM_AVG, STREAM_RST)
   !> @return    File prefix or empty string
   !---------------------------------------------------------------------------
   function variable_get_prefix(this, stream_index) result(prefix)
      class(io_variable), intent(in) :: this
      integer, intent(in) :: stream_index
      character(len=IO_PREFIX_LEN) :: prefix

      if (stream_index >= 1 .and. stream_index <= IO_NUM_FILE_TYPES) then
         prefix = this%streams(stream_index)%prefix
      else
         prefix = ""
      end if
   end function variable_get_prefix

   !---------------------------------------------------------------------------
   ! file_descriptor methods
   !---------------------------------------------------------------------------

   !---------------------------------------------------------------------------
   !> @brief Increment the time index for a file
   !>
   !> @param[inout] this  The file descriptor
   !---------------------------------------------------------------------------
   subroutine file_increment_time(this)
      class(file_descriptor), intent(inout) :: this
      this%time_index = this%time_index + 1
   end subroutine file_increment_time

   !---------------------------------------------------------------------------
   !> @brief Check if a file is open/valid
   !>
   !> @param[in] this  The file descriptor
   !> @return    True if backend_id is valid
   !---------------------------------------------------------------------------
   function file_is_open(this) result(is_open)
      class(file_descriptor), intent(in) :: this
      logical :: is_open
      is_open = (this%backend_id > 0)
   end function file_is_open

   !---------------------------------------------------------------------------
   ! io_var_registry methods
   !---------------------------------------------------------------------------

   !---------------------------------------------------------------------------
   !> @brief Add a variable to the registry
   !>
   !> @param[inout] this  The registry to modify
   !> @param[in]    var   The variable to add
   !---------------------------------------------------------------------------
   subroutine registry_add_variable(this, var)
      class(io_var_registry), intent(inout) :: this
      type(io_variable), intent(in) :: var
      type(io_variable), allocatable :: temp(:)
      integer :: i, new_size

      if (.not. allocated(this%variables)) then
         ! Initial allocation with extra space
         allocate(this%variables(IO_INITIAL_ALLOC))
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
         allocate(temp(new_size))

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

   !---------------------------------------------------------------------------
   !> @brief Find a variable by name
   !>
   !> @param[in]  this     The registry to search
   !> @param[in]  varname  Name of the variable to find
   !> @return     Index of the variable, or -1 if not found
   !---------------------------------------------------------------------------
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

   !---------------------------------------------------------------------------
   !> @brief Get the number of registered variables
   !>
   !> @param[in]  this  The registry to query
   !> @return     Number of variables in the registry
   !---------------------------------------------------------------------------
   function registry_size(this) result(sz)
      class(io_var_registry), intent(in) :: this
      integer :: sz
      sz = this%count
   end function registry_size

   !---------------------------------------------------------------------------
   !> @brief Get a variable by index (returns a copy)
   !>
   !> @param[in]  this  The registry to query
   !> @param[in]  idx   Index of the variable to retrieve
   !> @return     The requested variable (or uninitialized if invalid index)
   !---------------------------------------------------------------------------
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

   !---------------------------------------------------------------------------
   !> @brief Get a pointer to a variable by index (for modification)
   !>
   !> @param[inout] this  The registry to query
   !> @param[in]    idx   Index of the variable to retrieve
   !> @return       Pointer to the variable, or null if invalid index
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

   !---------------------------------------------------------------------------
   !> @brief Update a variable in the registry
   !>
   !> @param[inout] this  The registry to modify
   !> @param[in]    idx   Index of the variable to update
   !> @param[in]    var   New variable data
   !---------------------------------------------------------------------------
   subroutine registry_update_variable(this, idx, var)
      class(io_var_registry), intent(inout) :: this
      integer, intent(in) :: idx
      type(io_variable), intent(in) :: var

      if (allocated(this%variables) .and. idx > 0 .and. idx <= this%count) then
         this%variables(idx) = var
      end if
   end subroutine registry_update_variable

end module io_definitions
