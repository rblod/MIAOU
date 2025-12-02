!===============================================================================
!> @file io_definitions.F90
!>
!> @brief Core definitions for the I/O system
!>
!> This module defines the fundamental types and interfaces for the I/O system.
!> It uses composition to separate concerns into specialized types:
!>
!> - `var_metadata` — Variable identification and description
!> - `var_data_ptr` — Pointers to actual data arrays
!> - `avg_buffer` — Buffers for time averaging
!> - `output_stream` — Configuration for a single output stream
!> - `io_variable` — Composed type bringing everything together
!> - `file_descriptor` — Output file state
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

   ! Public types
   public :: var_metadata
   public :: var_data_ptr
   public :: avg_buffer
   public :: output_stream
   public :: io_variable
   public :: file_descriptor
   public :: io_var_registry

   ! Public constants for stream indices
   integer, public, parameter :: STREAM_HIS = 1  !< Index for history stream
   integer, public, parameter :: STREAM_AVG = 2  !< Index for average stream
   integer, public, parameter :: STREAM_RST = 3  !< Index for restart stream

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
   !>
   !> ## Example
   !>
   !> ```fortran
   !> type(var_data_ptr) :: data
   !> data%d2 => my_2d_array
   !> if (data%is_valid(2)) print *, "2D data available"
   !> ```
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
   !> @brief Buffer for time averaging
   !>
   !> Manages accumulation buffers for computing time averages. Handles
   !> initialization, accumulation, average computation, and reset.
   !>
   !> ## Example
   !>
   !> ```fortran
   !> type(avg_buffer) :: avg
   !> call avg%init(2, [nx, ny])      ! Initialize for 2D
   !> call avg%accumulate_2d(data)    ! Add values
   !> call avg%compute_2d(result)     ! Get average
   !> call avg%reset()                ! Clear for next period
   !> ```
   !---------------------------------------------------------------------------
   type :: avg_buffer
      real :: scalar = 0.0                      !< Accumulator for scalar values
      real, allocatable :: d1(:)                !< Accumulator for 1D arrays
      real, allocatable :: d2(:,:)              !< Accumulator for 2D arrays
      real, allocatable :: d3(:,:,:)            !< Accumulator for 3D arrays
      integer :: count = 0                      !< Number of accumulated samples
      logical :: initialized = .false.          !< True if buffers are allocated
   contains
      !> @brief Initialize buffers for given dimensions
      procedure :: init => avg_init
      !> @brief Check if ready to compute average
      procedure :: is_ready => avg_is_ready
      !> @brief Get accumulation count
      procedure :: get_count => avg_get_count
      !> @brief Reset buffers to zero
      procedure :: reset => avg_reset
      !> @brief Accumulate scalar value
      procedure :: accumulate_scalar => avg_accumulate_scalar
      !> @brief Accumulate 1D array
      procedure :: accumulate_1d => avg_accumulate_1d
      !> @brief Accumulate 2D array
      procedure :: accumulate_2d => avg_accumulate_2d
      !> @brief Accumulate 3D array
      procedure :: accumulate_3d => avg_accumulate_3d
      !> @brief Compute scalar average
      procedure :: compute_scalar => avg_compute_scalar
      !> @brief Compute 1D average
      procedure :: compute_1d => avg_compute_1d
      !> @brief Compute 2D average
      procedure :: compute_2d => avg_compute_2d
      !> @brief Compute 3D average
      procedure :: compute_3d => avg_compute_3d
   end type avg_buffer

   !---------------------------------------------------------------------------
   !> @brief Configuration for a single output stream
   !>
   !> An output stream represents one type of output (history, average, restart).
   !> Each variable can have multiple streams with independent configurations.
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
   !> @brief Complete I/O variable using composition
   !>
   !> This type composes all the specialized types to represent a complete
   !> variable for I/O operations. Each component handles a specific concern:
   !>
   !> - `meta` — What is this variable? (name, units, grid)
   !> - `data` — Where is the data? (pointers to arrays)
   !> - `avg` — Averaging state (buffers, count)
   !> - `streams` — How to output? (frequencies, prefixes)
   !>
   !> ## Example
   !>
   !> ```fortran
   !> type(io_variable) :: var
   !> var%meta%name = "zeta"
   !> var%meta%units = "m"
   !> var%meta%ndims = 2
   !> var%data%d2 => model_zeta
   !> var%streams(STREAM_HIS)%enabled = .true.
   !> ```
   !---------------------------------------------------------------------------
   type :: io_variable
      type(var_metadata) :: meta                         !< Variable identification
      type(var_data_ptr) :: data                         !< Pointers to data
      type(avg_buffer) :: avg                            !< Averaging buffers
      type(output_stream) :: streams(IO_NUM_FILE_TYPES)  !< Output configuration
   contains
      !> @brief Check if variable has any active output stream
      procedure :: has_output => variable_has_output
      !> @brief Check if variable requires averaging
      procedure :: needs_averaging => variable_needs_averaging
      !> @brief Get the file prefix for a given stream
      procedure :: get_prefix => variable_get_prefix
      !> @brief Initialize averaging buffers based on data shape
      procedure :: init_avg => variable_init_avg
      !> @brief Accumulate current data for averaging
      procedure :: accumulate => variable_accumulate
   end type io_variable

   !---------------------------------------------------------------------------
   !> @brief Descriptor for an output file
   !>
   !> Contains all state for an open output file, including backend-specific
   !> identifiers (e.g., NetCDF IDs).
   !---------------------------------------------------------------------------
   type :: file_descriptor
      character(len=IO_PATH_LEN) :: filename = ""   !< Complete filename
      character(len=IO_FILETYPE_LEN) :: type = ""   !< File type: "his", "avg", "rst"
      real :: freq = IO_FREQ_DISABLED               !< Write frequency (seconds)
      integer :: time_index = 1                     !< Current time write index

      ! Backend-specific identifiers (NetCDF)
      integer :: backend_id = -1            !< Backend file ID (e.g., NetCDF ncid)
      integer :: time_dimid = -1            !< Time dimension ID
      integer :: time_varid = -1            !< Time variable ID
   contains
      !> @brief Increment time index after writing
      procedure :: increment_time => file_increment_time
      !> @brief Check if file is valid/open
      procedure :: is_open => file_is_open
   end type file_descriptor

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
      procedure :: update => registry_update_variable
   end type io_var_registry

contains

   !===========================================================================
   ! var_data_ptr methods
   !===========================================================================

   !---------------------------------------------------------------------------
   !> @brief Check if data pointer is valid for given dimension
   !>
   !> @param[in] this   The data pointer container
   !> @param[in] ndims  Number of dimensions to check (0-3)
   !> @return    True if the corresponding pointer is associated
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
   !>
   !> @param[in]  this   The data pointer container
   !> @param[in]  ndims  Number of dimensions
   !> @param[out] shp    Array shape (size must be >= ndims)
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
   !>
   !> @param[inout] this  The data pointer container
   !---------------------------------------------------------------------------
   subroutine data_nullify_all(this)
      class(var_data_ptr), intent(inout) :: this

      nullify(this%scalar)
      nullify(this%d1)
      nullify(this%d2)
      nullify(this%d3)
   end subroutine data_nullify_all

   !===========================================================================
   ! avg_buffer methods
   !===========================================================================

   !---------------------------------------------------------------------------
   !> @brief Initialize averaging buffers
   !>
   !> @param[inout] this   The averaging buffer
   !> @param[in]    ndims  Number of dimensions
   !> @param[in]    shp    Shape array (ignored for scalar)
   !---------------------------------------------------------------------------
   subroutine avg_init(this, ndims, shp)
      class(avg_buffer), intent(inout) :: this
      integer, intent(in) :: ndims
      integer, intent(in), optional :: shp(:)

      ! Deallocate existing buffers
      if (allocated(this%d1)) deallocate(this%d1)
      if (allocated(this%d2)) deallocate(this%d2)
      if (allocated(this%d3)) deallocate(this%d3)

      select case (ndims)
      case (0)
         this%scalar = 0.0
      case (1)
         if (present(shp)) then
            allocate(this%d1(shp(1)))
            this%d1 = 0.0
         end if
      case (2)
         if (present(shp)) then
            allocate(this%d2(shp(1), shp(2)))
            this%d2 = 0.0
         end if
      case (3)
         if (present(shp)) then
            allocate(this%d3(shp(1), shp(2), shp(3)))
            this%d3 = 0.0
         end if
      end select

      this%count = 0
      this%initialized = .true.
   end subroutine avg_init

   !---------------------------------------------------------------------------
   !> @brief Check if average is ready to be computed
   !>
   !> @param[in] this  The averaging buffer
   !> @return    True if count > 0 and initialized
   !---------------------------------------------------------------------------
   function avg_is_ready(this) result(is_ready)
      class(avg_buffer), intent(in) :: this
      logical :: is_ready

      is_ready = (this%initialized .and. this%count > 0)
   end function avg_is_ready

   !---------------------------------------------------------------------------
   !> @brief Get the accumulation count
   !>
   !> @param[in] this  The averaging buffer
   !> @return    Number of accumulated samples
   !---------------------------------------------------------------------------
   function avg_get_count(this) result(count)
      class(avg_buffer), intent(in) :: this
      integer :: count

      count = this%count
   end function avg_get_count

   !---------------------------------------------------------------------------
   !> @brief Reset buffers to zero
   !>
   !> @param[inout] this  The averaging buffer
   !---------------------------------------------------------------------------
   subroutine avg_reset(this)
      class(avg_buffer), intent(inout) :: this

      this%scalar = 0.0
      if (allocated(this%d1)) this%d1 = 0.0
      if (allocated(this%d2)) this%d2 = 0.0
      if (allocated(this%d3)) this%d3 = 0.0
      this%count = 0
   end subroutine avg_reset

   !---------------------------------------------------------------------------
   !> @brief Accumulate a scalar value
   !>
   !> @param[inout] this   The averaging buffer
   !> @param[in]    value  Value to accumulate
   !---------------------------------------------------------------------------
   subroutine avg_accumulate_scalar(this, value)
      class(avg_buffer), intent(inout) :: this
      real, intent(in) :: value

      this%scalar = this%scalar + value
      this%count = this%count + 1
   end subroutine avg_accumulate_scalar

   !---------------------------------------------------------------------------
   !> @brief Accumulate a 1D array
   !>
   !> @param[inout] this  The averaging buffer
   !> @param[in]    arr   Array to accumulate
   !---------------------------------------------------------------------------
   subroutine avg_accumulate_1d(this, arr)
      class(avg_buffer), intent(inout) :: this
      real, intent(in) :: arr(:)

      if (allocated(this%d1)) then
         this%d1 = this%d1 + arr
         this%count = this%count + 1
      end if
   end subroutine avg_accumulate_1d

   !---------------------------------------------------------------------------
   !> @brief Accumulate a 2D array
   !>
   !> @param[inout] this  The averaging buffer
   !> @param[in]    arr   Array to accumulate
   !---------------------------------------------------------------------------
   subroutine avg_accumulate_2d(this, arr)
      class(avg_buffer), intent(inout) :: this
      real, intent(in) :: arr(:,:)

      if (allocated(this%d2)) then
         this%d2 = this%d2 + arr
         this%count = this%count + 1
      end if
   end subroutine avg_accumulate_2d

   !---------------------------------------------------------------------------
   !> @brief Accumulate a 3D array
   !>
   !> @param[inout] this  The averaging buffer
   !> @param[in]    arr   Array to accumulate
   !---------------------------------------------------------------------------
   subroutine avg_accumulate_3d(this, arr)
      class(avg_buffer), intent(inout) :: this
      real, intent(in) :: arr(:,:,:)

      if (allocated(this%d3)) then
         this%d3 = this%d3 + arr
         this%count = this%count + 1
      end if
   end subroutine avg_accumulate_3d

   !---------------------------------------------------------------------------
   !> @brief Compute scalar average
   !>
   !> @param[in]  this   The averaging buffer
   !> @param[out] avg    Computed average
   !> @return     True if successful
   !---------------------------------------------------------------------------
   function avg_compute_scalar(this, avg) result(success)
      class(avg_buffer), intent(in) :: this
      real, intent(out) :: avg
      logical :: success

      success = .false.
      avg = 0.0
      if (this%count > 0) then
         avg = this%scalar / real(this%count)
         success = .true.
      end if
   end function avg_compute_scalar

   !---------------------------------------------------------------------------
   !> @brief Compute 1D average
   !>
   !> @param[in]  this   The averaging buffer
   !> @param[out] avg    Computed average array
   !> @return     True if successful
   !---------------------------------------------------------------------------
   function avg_compute_1d(this, avg) result(success)
      class(avg_buffer), intent(in) :: this
      real, intent(out) :: avg(:)
      logical :: success

      success = .false.
      if (this%count > 0 .and. allocated(this%d1)) then
         avg = this%d1 / real(this%count)
         success = .true.
      end if
   end function avg_compute_1d

   !---------------------------------------------------------------------------
   !> @brief Compute 2D average
   !>
   !> @param[in]  this   The averaging buffer
   !> @param[out] avg    Computed average array
   !> @return     True if successful
   !---------------------------------------------------------------------------
   function avg_compute_2d(this, avg) result(success)
      class(avg_buffer), intent(in) :: this
      real, intent(out) :: avg(:,:)
      logical :: success

      success = .false.
      if (this%count > 0 .and. allocated(this%d2)) then
         avg = this%d2 / real(this%count)
         success = .true.
      end if
   end function avg_compute_2d

   !---------------------------------------------------------------------------
   !> @brief Compute 3D average
   !>
   !> @param[in]  this   The averaging buffer
   !> @param[out] avg    Computed average array
   !> @return     True if successful
   !---------------------------------------------------------------------------
   function avg_compute_3d(this, avg) result(success)
      class(avg_buffer), intent(in) :: this
      real, intent(out) :: avg(:,:,:)
      logical :: success

      success = .false.
      if (this%count > 0 .and. allocated(this%d3)) then
         avg = this%d3 / real(this%count)
         success = .true.
      end if
   end function avg_compute_3d

   !===========================================================================
   ! output_stream methods
   !===========================================================================

   !---------------------------------------------------------------------------
   !> @brief Check if output should occur at the given time
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

      if (.not. this%enabled) return
      if (this%frequency <= 0.0) return

      if (abs(current_time) < IO_TIME_TOLERANCE) then
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

   !===========================================================================
   ! io_variable methods
   !===========================================================================

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
   !> @param[in] this          The variable
   !> @param[in] stream_index  Stream index
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
   !> @brief Initialize averaging buffers based on data shape
   !>
   !> @param[inout] this  The variable
   !---------------------------------------------------------------------------
   subroutine variable_init_avg(this)
      class(io_variable), intent(inout) :: this
      integer :: shp(3)

      if (this%avg%initialized) return

      select case (this%meta%ndims)
      case (0)
         call this%avg%init(0)
      case (1)
         if (this%data%is_valid(1)) then
            call this%data%get_shape(1, shp)
            call this%avg%init(1, shp(1:1))
         end if
      case (2)
         if (this%data%is_valid(2)) then
            call this%data%get_shape(2, shp)
            call this%avg%init(2, shp(1:2))
         end if
      case (3)
         if (this%data%is_valid(3)) then
            call this%data%get_shape(3, shp)
            call this%avg%init(3, shp(1:3))
         end if
      end select
   end subroutine variable_init_avg

   !---------------------------------------------------------------------------
   !> @brief Accumulate current data for averaging
   !>
   !> @param[inout] this  The variable
   !---------------------------------------------------------------------------
   subroutine variable_accumulate(this)
      class(io_variable), intent(inout) :: this

      if (.not. this%needs_averaging()) return

      ! Initialize if needed
      if (.not. this%avg%initialized) call this%init_avg()

      ! Accumulate based on dimensionality
      select case (this%meta%ndims)
      case (0)
         if (this%data%is_valid(0)) then
            call this%avg%accumulate_scalar(this%data%scalar)
         end if
      case (1)
         if (this%data%is_valid(1)) then
            call this%avg%accumulate_1d(this%data%d1)
         end if
      case (2)
         if (this%data%is_valid(2)) then
            call this%avg%accumulate_2d(this%data%d2)
         end if
      case (3)
         if (this%data%is_valid(3)) then
            call this%avg%accumulate_3d(this%data%d3)
         end if
      end select
   end subroutine variable_accumulate

   !===========================================================================
   ! file_descriptor methods
   !===========================================================================

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

   !===========================================================================
   ! io_var_registry methods
   !===========================================================================

   !---------------------------------------------------------------------------
   !> @brief Add a variable to the registry
   !>
   !> @param[inout] this  The registry
   !> @param[in]    var   Variable to add
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
   !>
   !> @param[in]  this     The registry
   !> @param[in]  varname  Name to search for
   !> @return     Index of variable, or -1 if not found
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
   !>
   !> @param[in] this  The registry
   !> @return    Count of variables
   !---------------------------------------------------------------------------
   function registry_size(this) result(sz)
      class(io_var_registry), intent(in) :: this
      integer :: sz
      sz = this%count
   end function registry_size

   !---------------------------------------------------------------------------
   !> @brief Get a variable by index (returns a copy)
   !>
   !> @param[in] this  The registry
   !> @param[in] idx   Index
   !> @return    Copy of the variable
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
   !>
   !> @param[inout] this  The registry
   !> @param[in]    idx   Index
   !> @return       Pointer to variable, or null
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
   !> @param[inout] this  The registry
   !> @param[in]    idx   Index
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
