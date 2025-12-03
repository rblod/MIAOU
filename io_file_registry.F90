!===============================================================================
!> @file io_file_registry.F90
!>
!> @brief Output file definitions and registry
!>
!> This module manages output file definitions. Instead of attaching output
!> configuration to variables, we define output files and list which variables
!> go into each file. This approach:
!>
!> - Allows the same variable in multiple files with different frequencies
!> - Separates "what a variable is" from "how it's output"
!> - Aligns with XIOS/ADIOS2 configuration paradigms
!>
!> ## Example configuration
!>
!> ```fortran
!> type(output_file_def) :: hourly, daily_avg
!>
!> hourly%name = "surface_hourly"
!> hourly%frequency = 3600.0
!> hourly%operation = OP_INSTANT
!> hourly%variables = ["zeta", "u", "v"]
!>
!> daily_avg%name = "daily_mean"
!> daily_avg%frequency = 86400.0
!> daily_avg%operation = OP_AVERAGE
!> daily_avg%variables = ["zeta", "temp"]
!> ```
!>
!> @author Rachid Benshila
!> @date 2025-04
!===============================================================================
module io_file_registry
   use io_constants, only: IO_VARNAME_LEN, IO_PATH_LEN, IO_PREFIX_LEN, &
                           IO_FREQ_DISABLED
   implicit none
   private

   ! Public types
   public :: output_file_def
   public :: output_file_registry
   public :: avg_state

   ! Public constants for operations
   integer, public, parameter :: OP_INSTANT = 1    !< Instantaneous values
   integer, public, parameter :: OP_AVERAGE = 2    !< Time average
   integer, public, parameter :: OP_MIN = 3        !< Minimum over period
   integer, public, parameter :: OP_MAX = 4        !< Maximum over period
   integer, public, parameter :: OP_ACCUMULATE = 5 !< Accumulation (sum)

   ! Maximum variables per file
   integer, parameter :: MAX_VARS_PER_FILE = 50
   integer, parameter :: MAX_OUTPUT_FILES = 20
   integer, parameter :: INITIAL_FILE_ALLOC = 10

   !---------------------------------------------------------------------------
   !> @brief State for averaging/accumulation operations
   !>
   !> Each (file, variable) pair that needs averaging has its own state.
   !---------------------------------------------------------------------------
   type :: avg_state
      character(len=IO_VARNAME_LEN) :: var_name = ""  !< Variable name
      integer :: operation = OP_INSTANT               !< Operation type
      real :: scalar = 0.0                            !< Scalar accumulator
      real, allocatable :: d1(:)                      !< 1D accumulator
      real, allocatable :: d2(:,:)                    !< 2D accumulator
      real, allocatable :: d3(:,:,:)                  !< 3D accumulator
      integer :: count = 0                            !< Sample count
      logical :: initialized = .false.                !< Buffers allocated?
   contains
      procedure :: init => avg_state_init
      procedure :: reset => avg_state_reset
      procedure :: accumulate_scalar => avg_state_accumulate_scalar
      procedure :: accumulate_1d => avg_state_accumulate_1d
      procedure :: accumulate_2d => avg_state_accumulate_2d
      procedure :: accumulate_3d => avg_state_accumulate_3d
      procedure :: compute_scalar => avg_state_compute_scalar
      procedure :: compute_1d => avg_state_compute_1d
      procedure :: compute_2d => avg_state_compute_2d
      procedure :: compute_3d => avg_state_compute_3d
      procedure :: is_ready => avg_state_is_ready
   end type avg_state

   !---------------------------------------------------------------------------
   !> @brief Definition of an output file
   !>
   !> Describes what goes into an output file: which variables, at what
   !> frequency, with what operation (instant, average, etc.)
   !>
   !> ## Restart mode
   !>
   !> When `is_restart = .true.`:
   !> - File is written in overwrite mode (not append)
   !> - `restart_nlevels` consecutive time steps are written
   !> - `frequency < 0` means write only at final time
   !> - `double_precision = .true.` writes in real*8
   !> - Initial state (t=0) is never written
   !---------------------------------------------------------------------------
   type :: output_file_def
      ! Identification
      character(len=64) :: name = ""                  !< Unique identifier
      character(len=IO_PREFIX_LEN) :: prefix = ""     !< Filename prefix (empty = global)
      
      ! Output configuration
      real :: frequency = IO_FREQ_DISABLED            !< Output frequency (seconds), <0 = final only
      integer :: operation = OP_INSTANT               !< Operation type
      
      ! Restart-specific options
      logical :: is_restart = .false.                 !< Restart file mode
      integer :: restart_nlevels = 1                  !< Number of time levels to write
      logical :: double_precision = .false.           !< Write in double precision
      
      ! Variables in this file
      character(len=IO_VARNAME_LEN) :: variables(MAX_VARS_PER_FILE) = ""
      integer :: num_variables = 0
      
      ! Runtime state
      character(len=IO_PATH_LEN) :: filename = ""     !< Generated filename
      integer :: backend_id = -1                      !< Backend file handle
      integer :: time_dimid = -1                      !< Time dimension ID
      integer :: time_varid = -1                      !< Time variable ID
      integer :: time_index = 1                       !< Current time index
      
      ! Averaging state (one per variable)
      type(avg_state), allocatable :: avg_states(:)
      
      ! Restart buffer: stores time values for the last N levels
      real, allocatable :: restart_times(:)           !< Circular buffer for time values
      integer :: restart_buffer_idx = 0               !< Current index in circular buffer
      integer :: restart_buffer_count = 0             !< Number of values stored
   contains
      procedure :: add_variable => file_add_variable
      procedure :: has_variable => file_has_variable
      procedure :: needs_averaging => file_needs_averaging
      procedure :: get_avg_state => file_get_avg_state
      procedure :: init_avg_states => file_init_avg_states
      procedure :: is_open => file_is_open
      procedure :: increment_time => file_increment_time
      procedure :: is_restart_file => file_is_restart
      procedure :: init_restart_buffer => file_init_restart_buffer
      procedure :: store_restart_time => file_store_restart_time
   end type output_file_def

   !---------------------------------------------------------------------------
   !> @brief Registry of all output files
   !---------------------------------------------------------------------------
   type :: output_file_registry
      private
      type(output_file_def), allocatable :: files(:)
      integer :: count = 0
   contains
      procedure :: add => registry_add_file
      procedure :: find => registry_find_file
      procedure :: get => registry_get_file
      procedure :: get_ptr => registry_get_file_ptr
      procedure :: size => registry_size
      procedure :: get_files_for_variable => registry_files_for_var
   end type output_file_registry

contains

   !===========================================================================
   ! avg_state methods
   !===========================================================================

   !---------------------------------------------------------------------------
   !> @brief Initialize averaging state for given dimensions
   !---------------------------------------------------------------------------
   subroutine avg_state_init(this, var_name, operation, ndims, shp)
      class(avg_state), intent(inout) :: this
      character(len=*), intent(in) :: var_name
      integer, intent(in) :: operation, ndims
      integer, intent(in), optional :: shp(:)

      this%var_name = var_name
      this%operation = operation

      ! Deallocate existing
      if (allocated(this%d1)) deallocate(this%d1)
      if (allocated(this%d2)) deallocate(this%d2)
      if (allocated(this%d3)) deallocate(this%d3)

      ! Allocate based on dimensions
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
   end subroutine avg_state_init

   !---------------------------------------------------------------------------
   !> @brief Reset averaging state
   !---------------------------------------------------------------------------
   subroutine avg_state_reset(this)
      class(avg_state), intent(inout) :: this

      this%scalar = 0.0
      if (allocated(this%d1)) this%d1 = 0.0
      if (allocated(this%d2)) this%d2 = 0.0
      if (allocated(this%d3)) this%d3 = 0.0
      this%count = 0
   end subroutine avg_state_reset

   !---------------------------------------------------------------------------
   !> @brief Check if ready to compute result
   !---------------------------------------------------------------------------
   function avg_state_is_ready(this) result(ready)
      class(avg_state), intent(in) :: this
      logical :: ready
      ready = (this%initialized .and. this%count > 0)
   end function avg_state_is_ready

   !---------------------------------------------------------------------------
   !> @brief Accumulate scalar value
   !---------------------------------------------------------------------------
   subroutine avg_state_accumulate_scalar(this, value)
      class(avg_state), intent(inout) :: this
      real, intent(in) :: value

      select case (this%operation)
      case (OP_AVERAGE, OP_ACCUMULATE)
         this%scalar = this%scalar + value
      case (OP_MIN)
         if (this%count == 0) then
            this%scalar = value
         else
            this%scalar = min(this%scalar, value)
         end if
      case (OP_MAX)
         if (this%count == 0) then
            this%scalar = value
         else
            this%scalar = max(this%scalar, value)
         end if
      end select
      this%count = this%count + 1
   end subroutine avg_state_accumulate_scalar

   !---------------------------------------------------------------------------
   !> @brief Accumulate 1D array
   !---------------------------------------------------------------------------
   subroutine avg_state_accumulate_1d(this, arr)
      class(avg_state), intent(inout) :: this
      real, intent(in) :: arr(:)

      if (.not. allocated(this%d1)) return

      select case (this%operation)
      case (OP_AVERAGE, OP_ACCUMULATE)
         this%d1 = this%d1 + arr
      case (OP_MIN)
         if (this%count == 0) then
            this%d1 = arr
         else
            this%d1 = min(this%d1, arr)
         end if
      case (OP_MAX)
         if (this%count == 0) then
            this%d1 = arr
         else
            this%d1 = max(this%d1, arr)
         end if
      end select
      this%count = this%count + 1
   end subroutine avg_state_accumulate_1d

   !---------------------------------------------------------------------------
   !> @brief Accumulate 2D array
   !---------------------------------------------------------------------------
   subroutine avg_state_accumulate_2d(this, arr)
      class(avg_state), intent(inout) :: this
      real, intent(in) :: arr(:,:)

      if (.not. allocated(this%d2)) return

      select case (this%operation)
      case (OP_AVERAGE, OP_ACCUMULATE)
         this%d2 = this%d2 + arr
      case (OP_MIN)
         if (this%count == 0) then
            this%d2 = arr
         else
            this%d2 = min(this%d2, arr)
         end if
      case (OP_MAX)
         if (this%count == 0) then
            this%d2 = arr
         else
            this%d2 = max(this%d2, arr)
         end if
      end select
      this%count = this%count + 1
   end subroutine avg_state_accumulate_2d

   !---------------------------------------------------------------------------
   !> @brief Accumulate 3D array
   !---------------------------------------------------------------------------
   subroutine avg_state_accumulate_3d(this, arr)
      class(avg_state), intent(inout) :: this
      real, intent(in) :: arr(:,:,:)

      if (.not. allocated(this%d3)) return

      select case (this%operation)
      case (OP_AVERAGE, OP_ACCUMULATE)
         this%d3 = this%d3 + arr
      case (OP_MIN)
         if (this%count == 0) then
            this%d3 = arr
         else
            this%d3 = min(this%d3, arr)
         end if
      case (OP_MAX)
         if (this%count == 0) then
            this%d3 = arr
         else
            this%d3 = max(this%d3, arr)
         end if
      end select
      this%count = this%count + 1
   end subroutine avg_state_accumulate_3d

   !---------------------------------------------------------------------------
   !> @brief Compute scalar result
   !---------------------------------------------------------------------------
   function avg_state_compute_scalar(this, result_val) result(success)
      class(avg_state), intent(in) :: this
      real, intent(out) :: result_val
      logical :: success

      success = .false.
      result_val = 0.0

      if (this%count == 0) return

      select case (this%operation)
      case (OP_AVERAGE)
         result_val = this%scalar / real(this%count)
      case (OP_ACCUMULATE, OP_MIN, OP_MAX)
         result_val = this%scalar
      end select
      success = .true.
   end function avg_state_compute_scalar

   !---------------------------------------------------------------------------
   !> @brief Compute 1D result
   !---------------------------------------------------------------------------
   function avg_state_compute_1d(this, result_arr) result(success)
      class(avg_state), intent(in) :: this
      real, intent(out) :: result_arr(:)
      logical :: success

      success = .false.
      if (this%count == 0 .or. .not. allocated(this%d1)) return

      select case (this%operation)
      case (OP_AVERAGE)
         result_arr = this%d1 / real(this%count)
      case (OP_ACCUMULATE, OP_MIN, OP_MAX)
         result_arr = this%d1
      end select
      success = .true.
   end function avg_state_compute_1d

   !---------------------------------------------------------------------------
   !> @brief Compute 2D result
   !---------------------------------------------------------------------------
   function avg_state_compute_2d(this, result_arr) result(success)
      class(avg_state), intent(in) :: this
      real, intent(out) :: result_arr(:,:)
      logical :: success

      success = .false.
      if (this%count == 0 .or. .not. allocated(this%d2)) return

      select case (this%operation)
      case (OP_AVERAGE)
         result_arr = this%d2 / real(this%count)
      case (OP_ACCUMULATE, OP_MIN, OP_MAX)
         result_arr = this%d2
      end select
      success = .true.
   end function avg_state_compute_2d

   !---------------------------------------------------------------------------
   !> @brief Compute 3D result
   !---------------------------------------------------------------------------
   function avg_state_compute_3d(this, result_arr) result(success)
      class(avg_state), intent(in) :: this
      real, intent(out) :: result_arr(:,:,:)
      logical :: success

      success = .false.
      if (this%count == 0 .or. .not. allocated(this%d3)) return

      select case (this%operation)
      case (OP_AVERAGE)
         result_arr = this%d3 / real(this%count)
      case (OP_ACCUMULATE, OP_MIN, OP_MAX)
         result_arr = this%d3
      end select
      success = .true.
   end function avg_state_compute_3d

   !===========================================================================
   ! output_file_def methods
   !===========================================================================

   !---------------------------------------------------------------------------
   !> @brief Add a variable to this file
   !---------------------------------------------------------------------------
   subroutine file_add_variable(this, var_name)
      class(output_file_def), intent(inout) :: this
      character(len=*), intent(in) :: var_name

      if (this%num_variables >= MAX_VARS_PER_FILE) then
         print *, "ERROR: Too many variables in file ", trim(this%name)
         return
      end if

      ! Check for duplicates
      if (this%has_variable(var_name)) return

      this%num_variables = this%num_variables + 1
      this%variables(this%num_variables) = var_name
   end subroutine file_add_variable

   !---------------------------------------------------------------------------
   !> @brief Check if file contains a variable
   !---------------------------------------------------------------------------
   function file_has_variable(this, var_name) result(has_var)
      class(output_file_def), intent(in) :: this
      character(len=*), intent(in) :: var_name
      logical :: has_var
      integer :: i

      has_var = .false.
      do i = 1, this%num_variables
         if (trim(this%variables(i)) == trim(var_name)) then
            has_var = .true.
            return
         end if
      end do
   end function file_has_variable

   !---------------------------------------------------------------------------
   !> @brief Check if this file needs time averaging
   !---------------------------------------------------------------------------
   function file_needs_averaging(this) result(needs_avg)
      class(output_file_def), intent(in) :: this
      logical :: needs_avg

      needs_avg = (this%operation /= OP_INSTANT)
   end function file_needs_averaging

   !---------------------------------------------------------------------------
   !> @brief Get averaging state for a variable
   !---------------------------------------------------------------------------
   function file_get_avg_state(this, var_name) result(state_ptr)
      class(output_file_def), intent(inout), target :: this
      character(len=*), intent(in) :: var_name
      type(avg_state), pointer :: state_ptr
      integer :: i

      nullify(state_ptr)
      if (.not. allocated(this%avg_states)) return

      do i = 1, size(this%avg_states)
         if (trim(this%avg_states(i)%var_name) == trim(var_name)) then
            state_ptr => this%avg_states(i)
            return
         end if
      end do
   end function file_get_avg_state

   !---------------------------------------------------------------------------
   !> @brief Initialize averaging states for all variables
   !---------------------------------------------------------------------------
   subroutine file_init_avg_states(this)
      class(output_file_def), intent(inout) :: this
      integer :: i

      if (this%operation == OP_INSTANT) return
      if (allocated(this%avg_states)) deallocate(this%avg_states)

      allocate(this%avg_states(this%num_variables))

      do i = 1, this%num_variables
         this%avg_states(i)%var_name = this%variables(i)
         this%avg_states(i)%operation = this%operation
      end do
   end subroutine file_init_avg_states

   !---------------------------------------------------------------------------
   !> @brief Check if file is open
   !---------------------------------------------------------------------------
   function file_is_open(this) result(is_open)
      class(output_file_def), intent(in) :: this
      logical :: is_open
      is_open = (this%backend_id > 0)
   end function file_is_open

   !---------------------------------------------------------------------------
   !> @brief Increment time index
   !---------------------------------------------------------------------------
   subroutine file_increment_time(this)
      class(output_file_def), intent(inout) :: this
      this%time_index = this%time_index + 1
   end subroutine file_increment_time

   !===========================================================================
   ! output_file_registry methods
   !===========================================================================

   !---------------------------------------------------------------------------
   !> @brief Add a file to the registry
   !---------------------------------------------------------------------------
   subroutine registry_add_file(this, file_def)
      class(output_file_registry), intent(inout) :: this
      type(output_file_def), intent(in) :: file_def
      type(output_file_def), allocatable :: temp(:)
      integer :: new_size

      if (.not. allocated(this%files)) then
         allocate(this%files(INITIAL_FILE_ALLOC))
         this%count = 0
      end if

      ! Expand if needed
      if (this%count >= size(this%files)) then
         new_size = size(this%files) * 2
         allocate(temp(new_size))
         temp(1:this%count) = this%files(1:this%count)
         call move_alloc(temp, this%files)
      end if

      this%count = this%count + 1
      this%files(this%count) = file_def
   end subroutine registry_add_file

   !---------------------------------------------------------------------------
   !> @brief Find a file by name
   !---------------------------------------------------------------------------
   function registry_find_file(this, name) result(idx)
      class(output_file_registry), intent(in) :: this
      character(len=*), intent(in) :: name
      integer :: idx, i

      idx = -1
      if (.not. allocated(this%files)) return

      do i = 1, this%count
         if (trim(this%files(i)%name) == trim(name)) then
            idx = i
            return
         end if
      end do
   end function registry_find_file

   !---------------------------------------------------------------------------
   !> @brief Get a file by index (copy)
   !---------------------------------------------------------------------------
   function registry_get_file(this, idx) result(file_def)
      class(output_file_registry), intent(in) :: this
      integer, intent(in) :: idx
      type(output_file_def) :: file_def

      if (allocated(this%files) .and. idx > 0 .and. idx <= this%count) then
         file_def = this%files(idx)
      end if
   end function registry_get_file

   !---------------------------------------------------------------------------
   !> @brief Get a pointer to a file by index
   !---------------------------------------------------------------------------
   function registry_get_file_ptr(this, idx) result(file_ptr)
      class(output_file_registry), intent(inout), target :: this
      integer, intent(in) :: idx
      type(output_file_def), pointer :: file_ptr

      nullify(file_ptr)
      if (allocated(this%files) .and. idx > 0 .and. idx <= this%count) then
         file_ptr => this%files(idx)
      end if
   end function registry_get_file_ptr

   !---------------------------------------------------------------------------
   !> @brief Get number of files
   !---------------------------------------------------------------------------
   function registry_size(this) result(sz)
      class(output_file_registry), intent(in) :: this
      integer :: sz
      sz = this%count
   end function registry_size

   !---------------------------------------------------------------------------
   !> @brief Get all files containing a variable
   !>
   !> @param[in]  this      The registry
   !> @param[in]  var_name  Variable name to search for
   !> @param[out] indices   Array of file indices (must be pre-allocated)
   !> @param[out] num_found Number of files found
   !---------------------------------------------------------------------------
   subroutine registry_files_for_var(this, var_name, indices, num_found)
      class(output_file_registry), intent(in) :: this
      character(len=*), intent(in) :: var_name
      integer, intent(out) :: indices(:)
      integer, intent(out) :: num_found
      integer :: i

      num_found = 0
      if (.not. allocated(this%files)) return

      do i = 1, this%count
         if (this%files(i)%has_variable(var_name)) then
            num_found = num_found + 1
            if (num_found <= size(indices)) then
               indices(num_found) = i
            end if
         end if
      end do
   end subroutine registry_files_for_var

   !===========================================================================
   ! Restart-specific methods
   !===========================================================================

   !---------------------------------------------------------------------------
   !> @brief Check if this is a restart file
   !---------------------------------------------------------------------------
   function file_is_restart(this) result(is_rst)
      class(output_file_def), intent(in) :: this
      logical :: is_rst
      is_rst = this%is_restart
   end function file_is_restart

   !---------------------------------------------------------------------------
   !> @brief Initialize restart buffer for time values
   !---------------------------------------------------------------------------
   subroutine file_init_restart_buffer(this)
      class(output_file_def), intent(inout) :: this

      if (.not. this%is_restart) return
      
      if (allocated(this%restart_times)) deallocate(this%restart_times)
      allocate(this%restart_times(this%restart_nlevels))
      this%restart_times = 0.0
      this%restart_buffer_idx = 0
      this%restart_buffer_count = 0
   end subroutine file_init_restart_buffer

   !---------------------------------------------------------------------------
   !> @brief Store a time value in the restart circular buffer
   !>
   !> @param[in] time_value  Time to store
   !---------------------------------------------------------------------------
   subroutine file_store_restart_time(this, time_value)
      class(output_file_def), intent(inout) :: this
      real, intent(in) :: time_value

      if (.not. this%is_restart) return
      if (.not. allocated(this%restart_times)) call this%init_restart_buffer()

      ! Circular buffer: advance index
      this%restart_buffer_idx = mod(this%restart_buffer_idx, this%restart_nlevels) + 1
      this%restart_times(this%restart_buffer_idx) = time_value
      
      ! Track how many values we have (up to nlevels)
      if (this%restart_buffer_count < this%restart_nlevels) then
         this%restart_buffer_count = this%restart_buffer_count + 1
      end if
   end subroutine file_store_restart_time

end module io_file_registry
