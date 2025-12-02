!===============================================================================
!> @file io_manager.F90
!>
!> High-level I/O management module
!>
!> This module provides a unified interface for I/O operations, independent
!> of the specific backend implementation. It serves as the central coordinator
!> for variable registration, file management, and data output.
!>
!> REFACTORED:
!> - Single file registry (removed duplication with io_netcdf)
!> - Use pointers to modify variables in registry (fix avg accumulation bug)
!> - Improved error handling with status propagation
!>
!> @author Rachid Benshila
!> @date 2025-04-11
!===============================================================================
module io_manager
   use io_constants, only: IO_TIME_TOLERANCE, IO_PATH_LEN, IO_PREFIX_LEN, &
                           IO_NUM_FILE_TYPES, IO_INITIAL_ALLOC
   use io_definitions, only: io_variable, file_descriptor, io_var_registry, &
                             output_stream, STREAM_HIS, STREAM_AVG, STREAM_RST
   use io_config, only: read_io_config, apply_config_to_variable, get_output_prefix
   use io_naming, only: generate_filename
   use io_netcdf, only: nc_initialize, nc_finalize, nc_create_file, &
                        nc_write_variable_data, nc_write_time, &
                        nc_close_file, nc_define_variable_in_file, &
                        nc_end_definition, get_varid_for_variable
   use io_netcdf_avg, only: accumulate_avg, write_variable_avg, reset_avg

   implicit none
   private

   ! Public interface
   public :: initialize_io, finalize_io
   public :: register_variable, write_data, write_all_data
   public :: get_io_status

   ! Registry for variables - made public for direct access if needed
   type(io_var_registry), public, target :: var_registry

   ! Array of open files (single source of truth)
   type(file_descriptor), allocatable, target :: open_files(:)
   integer :: num_files = 0

   ! Time configuration storage
   character(len=IO_PATH_LEN), private :: stored_time_units = "seconds since 2023-01-01 00:00:00"
   character(len=IO_PATH_LEN), private :: stored_calendar = "gregorian"

   ! Module state
   logical, private :: is_initialized = .false.
   integer, private :: last_error = 0
   character(len=256), private :: last_error_msg = ""

contains

   !---------------------------------------------------------------------------
   ! Status and error handling
   !---------------------------------------------------------------------------

   !> Get the last error status
   !>
   !> @param[out] error_msg  Optional: retrieve error message
   !> @return     Last error code (0 = no error)
   function get_io_status(error_msg) result(status)
      character(len=*), intent(out), optional :: error_msg
      integer :: status

      status = last_error
      if (present(error_msg)) then
         error_msg = last_error_msg
      end if
   end function get_io_status

   !> Set error status
   !>
   !> @param[in] code  Error code
   !> @param[in] msg   Error message
   subroutine set_error(code, msg)
      integer, intent(in) :: code
      character(len=*), intent(in) :: msg

      last_error = code
      last_error_msg = msg
      if (code /= 0) then
         print *, "IO_MANAGER ERROR [", code, "]: ", trim(msg)
      end if
   end subroutine set_error

   !> Clear error status
   subroutine clear_error()
      last_error = 0
      last_error_msg = ""
   end subroutine clear_error

   !---------------------------------------------------------------------------
   ! Initialization and cleanup
   !---------------------------------------------------------------------------

   !> Initialize the I/O system
   !>
   !> @param[in] config_file  Optional: path to configuration file
   !> @param[in] time_units   Optional: units for time axis
   !> @param[in] calendar     Optional: calendar type for time
   !> @return    Status code (0 = success)
   function initialize_io(config_file, time_units, calendar) result(status)
      character(len=*), intent(in), optional :: config_file
      character(len=*), intent(in), optional :: time_units, calendar
      integer :: status

      character(len=256) :: config_path

      call clear_error()

      ! Set default configuration file path
      if (present(config_file)) then
         config_path = config_file
      else
         config_path = "output_config.nml"
      end if

      ! Store time units and calendar for later use
      if (present(time_units)) then
         stored_time_units = time_units
      end if

      if (present(calendar)) then
         stored_calendar = calendar
      end if

      ! Initialize the NetCDF backend
      status = nc_initialize()
      if (status /= 0) then
         call set_error(status, "Failed to initialize NetCDF backend")
         return
      end if

      ! Read configuration
      status = read_io_config(config_path)
      if (status /= 0) then
         print *, "Warning: Configuration read failed, using defaults"
         ! Not a fatal error, continue with defaults
      end if

      ! Initialize file registry
      if (allocated(open_files)) deallocate(open_files)
      num_files = 0

      is_initialized = .true.
      status = 0
   end function initialize_io

   !> Finalize the I/O system
   !>
   !> @return Status code (0 = success)
   function finalize_io() result(status)
      integer :: status, i, close_status

      status = 0

      ! Close all open files
      if (allocated(open_files)) then
         do i = 1, num_files
            if (open_files(i)%is_open()) then
               close_status = nc_close_file(open_files(i))
               if (close_status /= 0) then
                  status = -1  ! Record that at least one close failed
               end if
            end if
         end do
         deallocate(open_files)
         num_files = 0
      end if

      ! Finalize NetCDF backend
      close_status = nc_finalize()
      if (close_status /= 0) status = -1

      is_initialized = .false.
   end function finalize_io

   !---------------------------------------------------------------------------
   ! Variable registration
   !---------------------------------------------------------------------------

   !> Register a variable for output
   !>
   !> @param[in] var  Variable to register
   subroutine register_variable(var)
      type(io_variable), intent(in) :: var

      type(io_variable) :: var_copy

      ! Apply configuration to variable
      var_copy = var
      call apply_config_to_variable(var_copy)

      ! Add to registry
      call var_registry%add(var_copy)
   end subroutine register_variable

   !---------------------------------------------------------------------------
   ! File management
   !---------------------------------------------------------------------------

   !> Add a file to the registry of open files
   !>
   !> @param[in] file_desc  File descriptor to add
   subroutine add_to_open_files(file_desc)
      type(file_descriptor), intent(in) :: file_desc
      type(file_descriptor), allocatable :: temp(:)
      integer :: n

      if (.not. allocated(open_files)) then
         allocate(open_files(IO_INITIAL_ALLOC))  ! Pre-allocate space
         num_files = 1
         open_files(1) = file_desc
      else if (num_files >= size(open_files)) then
         ! Need to expand
         n = size(open_files)
         allocate(temp(n + IO_INITIAL_ALLOC))
         temp(1:n) = open_files
         call move_alloc(temp, open_files)
         num_files = num_files + 1
         open_files(num_files) = file_desc
      else
         num_files = num_files + 1
         open_files(num_files) = file_desc
      end if
   end subroutine add_to_open_files

   !> Check if a file exists in the registry
   !>
   !> @param[in] filename  Name of the file to check
   !> @return    True if file exists, false otherwise
   function file_exists(filename) result(exists)
      character(len=*), intent(in) :: filename
      logical :: exists
      integer :: i

      exists = .false.
      if (allocated(open_files)) then
         do i = 1, num_files
            if (trim(open_files(i)%filename) == trim(filename)) then
               exists = .true.
               return
            end if
         end do
      end if
   end function file_exists

   !> Get a pointer to an open file by index
   !>
   !> @param[in] idx  Index of the file
   !> @return    Pointer to file descriptor, or null if invalid
   function get_file_ptr(idx) result(file_ptr)
      integer, intent(in) :: idx
      type(file_descriptor), pointer :: file_ptr

      if (allocated(open_files) .and. idx > 0 .and. idx <= num_files) then
         file_ptr => open_files(idx)
      else
         nullify(file_ptr)
      end if
   end function get_file_ptr

   !> Create all necessary output files for registered variables
   !>
   !> @param[in] time_units  Optional: units for time axis
   !> @param[in] calendar    Optional: calendar type for time
   !> @return    Number of files created
   function create_output_files(time_units, calendar) result(files_created)
      character(len=*), intent(in), optional :: time_units, calendar
      integer :: files_created

      integer :: var_idx, stream_idx, status
      character(len=IO_PATH_LEN) :: filename, prefix
      type(io_variable), pointer :: var_ptr
      type(output_stream) :: stream
      type(file_descriptor) :: file_desc
      character(len=IO_PATH_LEN) :: local_time_units, local_calendar

      files_created = 0

      ! Use stored values or provided ones
      if (present(time_units)) then
         local_time_units = time_units
      else
         local_time_units = stored_time_units
      end if

      if (present(calendar)) then
         local_calendar = calendar
      else
         local_calendar = stored_calendar
      end if

      ! For each variable in the registry
      do var_idx = 1, var_registry%size()
         var_ptr => var_registry%get_ptr(var_idx)
         if (.not. associated(var_ptr)) cycle

         ! For each output stream
         do stream_idx = 1, IO_NUM_FILE_TYPES
            stream = var_ptr%streams(stream_idx)

            ! Skip inactive streams
            if (.not. stream%is_active()) cycle

            ! Determine prefix
            if (trim(stream%prefix) == "") then
               prefix = get_output_prefix()
            else
               prefix = stream%prefix
            end if

            ! Generate filename
            filename = generate_filename(prefix, stream%stream_type, stream%frequency)

            ! Check if file already exists in our list
            if (file_exists(filename)) cycle

            ! Create the file
            status = nc_create_file(filename, stream%stream_type, stream%frequency, &
                                   local_time_units, local_calendar, file_desc)

            if (status /= 0) then
               call set_error(status, "Failed to create file: "//trim(filename))
               cycle
            end if

            ! Define all applicable variables in this file
            call define_variables_in_file(file_desc, stream%stream_type)

            ! End definition mode
            status = nc_end_definition(file_desc)
            if (status /= 0) then
               call set_error(status, "Failed to end definition for: "//trim(filename))
            end if

            ! Add to our file list
            call add_to_open_files(file_desc)
            files_created = files_created + 1

            print *, "Created file: ", trim(filename)
         end do
      end do
   end function create_output_files

   !> Define all applicable variables in a file
   !>
   !> @param[in] file_desc  File descriptor
   !> @param[in] file_type  Type of file ("his", "avg", "rst")
   subroutine define_variables_in_file(file_desc, file_type)
      type(file_descriptor), intent(in) :: file_desc
      character(len=*), intent(in) :: file_type

      integer :: i, stream_idx, status
      type(io_variable), pointer :: var_ptr
      type(output_stream) :: stream
      character(len=IO_PREFIX_LEN) :: expected_prefix

      ! Determine stream index from file type
      stream_idx = get_stream_index(file_type)
      if (stream_idx < 1) return

      do i = 1, var_registry%size()
         var_ptr => var_registry%get_ptr(i)
         if (.not. associated(var_ptr)) cycle

         ! Get stream for this file type
         stream = var_ptr%streams(stream_idx)

         ! Skip if stream not active
         if (.not. stream%is_active()) cycle

         ! Determine expected prefix for this variable
         if (trim(stream%prefix) /= "") then
            expected_prefix = trim(stream%prefix)
         else
            expected_prefix = trim(get_output_prefix())
         end if

         ! Check if this variable belongs in this file (prefix match)
         if (index(file_desc%filename, trim(expected_prefix)) /= 1) cycle

         ! Define the variable in the file
         status = nc_define_variable_in_file(file_desc, var_ptr)
         if (status /= 0) then
            print *, "Warning: Failed to define variable ", trim(var_ptr%meta%name), &
                     " in file ", trim(file_desc%filename)
         end if
      end do
   end subroutine define_variables_in_file

   !> Get stream index from file type string
   !>
   !> @param[in] file_type  File type ("his", "avg", "rst")
   !> @return    Stream index or -1 if invalid
   function get_stream_index(file_type) result(idx)
      character(len=*), intent(in) :: file_type
      integer :: idx

      select case (trim(file_type))
      case ("his")
         idx = STREAM_HIS
      case ("avg")
         idx = STREAM_AVG
      case ("rst")
         idx = STREAM_RST
      case default
         idx = -1
      end select
   end function get_stream_index

   !---------------------------------------------------------------------------
   ! Data writing
   !---------------------------------------------------------------------------

   !> Write data for a specific variable at the current time
   !>
   !> @param[in] var_name     Name of the variable to write
   !> @param[in] current_time Current model time in seconds
   !> @return    Status code (0 = success)
   function write_data(var_name, current_time) result(status)
      character(len=*), intent(in) :: var_name
      real, intent(in) :: current_time
      integer :: status

      integer :: var_idx, file_idx
      type(io_variable), pointer :: var_ptr
      type(file_descriptor), pointer :: file_ptr

      status = -1

      ! Find the variable in the registry
      var_idx = var_registry%find(var_name)
      if (var_idx < 0) then
         call set_error(-1, "Variable not found: "//trim(var_name))
         return
      end if

      var_ptr => var_registry%get_ptr(var_idx)
      if (.not. associated(var_ptr)) return

      ! Process all open files
      do file_idx = 1, num_files
         file_ptr => get_file_ptr(file_idx)
         if (.not. associated(file_ptr)) cycle

         ! Determine if this variable should be written to this file
         if (should_write_to_file(var_ptr, file_ptr, current_time)) then
            status = nc_write_variable_data(file_ptr, var_ptr)
            if (status == 0) then
               status = nc_write_time(file_ptr, current_time)
            end if
         end if
      end do

      status = 0
   end function write_data

   !> Write all registered variables at the current time
   !>
   !> @param[in] current_time  Current model time in seconds
   !> @param[in] is_final_step Optional: flag indicating if this is the final time step
   !> @return    Status code (0 = success)
   function write_all_data(current_time, is_final_step) result(status)
      real, intent(in) :: current_time
      logical, intent(in), optional :: is_final_step
      integer :: status

      integer :: var_idx, file_idx
      type(io_variable), pointer :: var_ptr
      type(file_descriptor), pointer :: file_ptr
      logical :: final_step, any_written
      logical, allocatable :: is_write_time(:)

      status = 0
      final_step = .false.
      if (present(is_final_step)) final_step = is_final_step

      ! Create output files if they don't exist yet
      if (num_files == 0) then
         if (create_output_files() == 0) then
            call set_error(-1, "No output files created")
            status = -1
            return
         end if
      end if

      ! Pre-calculate write times for each file
      allocate(is_write_time(num_files))
      do file_idx = 1, num_files
         file_ptr => get_file_ptr(file_idx)
         if (.not. associated(file_ptr)) then
            is_write_time(file_idx) = .false.
            cycle
         end if

         ! For restart files, force writing on final step
         if (trim(file_ptr%type) == "rst" .and. final_step) then
            is_write_time(file_idx) = .true.
         else
            is_write_time(file_idx) = is_output_time(current_time, file_ptr%freq)
         end if
      end do

      ! First, accumulate averages for all variables (using pointers!)
      do var_idx = 1, var_registry%size()
         var_ptr => var_registry%get_ptr(var_idx)
         if (.not. associated(var_ptr)) cycle

         ! Only accumulate for variables that need averages
         if (var_ptr%needs_averaging()) then
            call accumulate_avg(var_ptr)
         end if
      end do

      ! Process all open files
      do file_idx = 1, num_files
         file_ptr => get_file_ptr(file_idx)
         if (.not. associated(file_ptr)) cycle

         any_written = .false.

         ! Process all variables for this file
         do var_idx = 1, var_registry%size()
            var_ptr => var_registry%get_ptr(var_idx)
            if (.not. associated(var_ptr)) cycle

            ! Write variable if needed
            call write_var_to_file(file_ptr, var_ptr, final_step, &
                                   is_write_time(file_idx), any_written)
         end do

         ! Write time value if any variables were written
         if (any_written) then
            status = nc_write_time(file_ptr, current_time)
         end if
      end do

      deallocate(is_write_time)
   end function write_all_data

   !> Write a variable to a file if appropriate
   !>
   !> @param[inout] file_ptr      Pointer to file descriptor
   !> @param[inout] var_ptr       Pointer to variable
   !> @param[in]    is_final_step Flag indicating final step
   !> @param[in]    is_write_time Flag indicating it's time to write
   !> @param[inout] any_written   Set to true if data was written
   subroutine write_var_to_file(file_ptr, var_ptr, is_final_step, is_write_time, any_written)
      type(file_descriptor), pointer, intent(inout) :: file_ptr
      type(io_variable), pointer, intent(inout) :: var_ptr
      logical, intent(in) :: is_final_step, is_write_time
      logical, intent(inout) :: any_written

      integer :: varid, stream_idx
      type(output_stream) :: stream

      ! Check if this variable belongs to this file
      if (.not. belongs_to_file(var_ptr, file_ptr)) return

      ! Get stream index for this file type
      stream_idx = get_stream_index(file_ptr%type)
      if (stream_idx < 1) return

      stream = var_ptr%streams(stream_idx)

      ! Skip if stream not active
      if (.not. stream%is_active()) return

      ! Case 1: Final step restart files
      if (is_final_step .and. stream_idx == STREAM_RST) then
         if (nc_write_variable_data(file_ptr, var_ptr) == 0) any_written = .true.
         return
      end if

      ! Case 2: Regular write time
      if (.not. is_write_time) return

      select case (stream_idx)
      case (STREAM_HIS)
         if (nc_write_variable_data(file_ptr, var_ptr) == 0) any_written = .true.

      case (STREAM_AVG)
         varid = get_varid_for_variable(var_ptr, file_ptr%backend_id)
         if (write_variable_avg(var_ptr, file_ptr%backend_id, varid, &
                                file_ptr%time_index) == 0) then
            any_written = .true.
            call reset_avg(var_ptr)
         end if

      case (STREAM_RST)
         if (nc_write_variable_data(file_ptr, var_ptr) == 0) any_written = .true.
      end select
   end subroutine write_var_to_file

   !---------------------------------------------------------------------------
   ! Helper functions
   !---------------------------------------------------------------------------

   !> Check if it's time to write based on frequency
   !>
   !> @param[in] current_time  Current model time
   !> @param[in] freq          Output frequency
   !> @return    True if it's time to write
   function is_output_time(current_time, freq) result(is_time)
      real, intent(in) :: current_time, freq
      logical :: is_time

      if (freq <= 0.0) then
         is_time = .false.
      else
         is_time = abs(mod(current_time, freq)) < IO_TIME_TOLERANCE
      end if
   end function is_output_time

   !> Determine if a variable should be written to a file at the current time
   !>
   !> @param[in] var_ptr       Pointer to variable
   !> @param[in] file_ptr      Pointer to file descriptor
   !> @param[in] current_time  Current model time
   !> @return    True if variable should be written to this file
   function should_write_to_file(var_ptr, file_ptr, current_time) result(should_write)
      type(io_variable), pointer, intent(in) :: var_ptr
      type(file_descriptor), pointer, intent(in) :: file_ptr
      real, intent(in) :: current_time
      logical :: should_write

      integer :: stream_idx
      type(output_stream) :: stream

      should_write = .false.

      ! First check if the variable belongs to this file
      if (.not. belongs_to_file(var_ptr, file_ptr)) return

      ! Get stream index for this file type
      stream_idx = get_stream_index(file_ptr%type)
      if (stream_idx < 1) return

      stream = var_ptr%streams(stream_idx)

      ! Check if stream is active
      if (.not. stream%is_active()) return

      ! Determine if we should write at this time step
      if (abs(current_time) < IO_TIME_TOLERANCE) then
         ! First time step - only write history
         should_write = (stream_idx == STREAM_HIS)
      else
         should_write = stream%should_write(current_time)
      end if
   end function should_write_to_file

   !> Check if a variable belongs to a file based on prefix
   !>
   !> @param[in] var_ptr   Pointer to variable
   !> @param[in] file_ptr  Pointer to file descriptor
   !> @return    True if the variable belongs to this file
   function belongs_to_file(var_ptr, file_ptr) result(belongs)
      type(io_variable), pointer, intent(in) :: var_ptr
      type(file_descriptor), pointer, intent(in) :: file_ptr
      logical :: belongs

      integer :: stream_idx
      character(len=IO_PREFIX_LEN) :: prefix

      belongs = .false.

      ! Get stream index for this file type
      stream_idx = get_stream_index(file_ptr%type)
      if (stream_idx < 1) return

      ! Determine expected prefix for this variable and stream
      prefix = var_ptr%get_prefix(stream_idx)
      if (trim(prefix) == "") then
         prefix = trim(get_output_prefix())
      end if

      ! Check if this file matches the expected prefix
      belongs = (index(file_ptr%filename, trim(prefix)) == 1)
   end function belongs_to_file

end module io_manager
