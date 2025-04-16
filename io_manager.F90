!===============================================================================
!> @file io_manager.F90
!>
!> High-level I/O management module
!>
!> This module provides a unified interface for I/O operations, independent
!> of the specific backend implementation. It serves as the central coordinator
!> for variable registration, file management, and data output.
!>
!> @author Rachid Benshila
!> @date 2025-04-11
!===============================================================================
module io_manager
   use io_definitions, only: io_variable, file_descriptor, io_var_registry
   use io_config, only: read_io_config, apply_config_to_variable, get_output_prefix
use io_netcdf, only: nc_initialize, nc_finalize, nc_create_file, &
                     nc_write_variable_data, nc_write_time, &
                     nc_close_file, generate_filename, nc_define_variable_in_file, &
                     nc_end_definition, get_varid_for_variable
 use io_netcdf_avg, only : accumulate_average, write_average, reset_average,&
 init_avg_buffers, accumulate_avg, write_variable_avg, reset_avg

   implicit none
   private

   ! Public interface
   public :: initialize_io, finalize_io
   public :: register_variable, write_data, write_all_data

   ! Registry for variables
   type(io_var_registry) :: var_registry

   ! Array of open files
   type(file_descriptor), allocatable :: open_files(:)

   ! File types
   character(len=*), parameter :: FILE_TYPES(3) = ["his", "avg", "rst"]

   ! Constants
   real, parameter :: TOLERANCE = 1.0e-5

   character(len=256), private :: stored_time_units = "seconds since 2023-01-01 00:00:00"
character(len=256), private :: stored_calendar = "gregorian"

contains

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
   
   ! Set default configuration file path
   if (present(config_file)) then
      config_path = config_file
   else
      config_path = "output_config.nml"
   end if
   
   ! Store time units and calendar for later use
   if (present(time_units)) then
      stored_time_units = time_units
      print *, "Using time units: ", trim(stored_time_units)
   end if
   
   if (present(calendar)) then
      stored_calendar = calendar
      print *, "Using calendar: ", trim(stored_calendar)
   end if
   
   ! Initialize the NetCDF backend
   status = nc_initialize()
   if (status /= 0) return
   
   ! Read configuration
   status = read_io_config(config_path)
   if (status /= 0) then
      print *, "Warning: Configuration read failed, using defaults"
   end if
   
   ! Create or clear file registry
   if (allocated(open_files)) deallocate(open_files)
   
   status = 0
end function initialize_io

   !> Finalize the I/O system
   !>
   !> @return Status code (0 = success)
   function finalize_io() result(status)
      integer :: status
      integer :: i
      
      ! Close all open files
      if (allocated(open_files)) then
         do i = 1, size(open_files)
            status = nc_close_file(open_files(i))
         end do
         
         deallocate(open_files)
      end if
      
      ! Finalize NetCDF backend
      status = nc_finalize()
   end function finalize_io

   !---------------------------------------------------------------------------
   ! Variable registration
   !---------------------------------------------------------------------------

   !> Register a variable for output
   !>
   !> @param[in] var  Variable to register
   !> @return    Variable index in registry
   subroutine register_variable(var)! result(var_idx)
      type(io_variable), intent(in) :: var
      integer :: var_idx
      
      type(io_variable) :: var_copy
      
      ! Apply configuration to variable
      var_copy = var
      call apply_config_to_variable(var_copy)
      
      ! Add to registry
      call var_registry%add(var_copy)
      
      ! Return variable index
      var_idx = var_registry%find(var%name)
   end subroutine register_variable

   !---------------------------------------------------------------------------
   ! File operations
   !---------------------------------------------------------------------------

   !> Create all necessary output files for registered variables
   !>
   !> @param[in] time_units  Optional: units for time axis
   !> @param[in] calendar    Optional: calendar type for time
   !> @return    Number of files created
!> Create all necessary output files for registered variables
!>
!> This function creates NetCDF output files for all registered variables
!> based on their output frequency and file type configuration. It also
!> defines all applicable variables in each created file.
!>
!> @param[in] time_units  Optional: units for time axis
!> @param[in] calendar    Optional: calendar type for time
!> @return    Number of files created
!> Create all necessary output files for registered variables
!>
!> This function creates NetCDF output files for all registered variables
!> based on their output frequency and file type configuration. It also
!> defines all applicable variables in each created file, respecting
!> variable-specific prefixes and output settings.
!>
!> @param[in] time_units  Optional: units for time axis
!> @param[in] calendar    Optional: calendar type for time
!> @return    Number of files created
function create_output_files(time_units, calendar) result(num_files)
   character(len=*), intent(in), optional :: time_units, calendar
   integer :: num_files
   
   integer :: i, j, var_idx, file_idx
   integer :: status
   real :: freq
   character(len=256) :: filename
   character(len=128) :: expected_prefix
   type(io_variable) :: var
   type(file_descriptor) :: file_desc
   logical :: file_exists
   
   num_files = 0
   
   ! For each variable in the registry
   do var_idx = 1, var_registry%size()
      var = var_registry%get(var_idx)
      
      ! For each output file type
      do j = 1, size(FILE_TYPES)
         ! Check if this variable should be written to this type of file
         select case (trim(FILE_TYPES(j)))
         case ("his")
            if (.not. var%to_his) cycle
            freq = var%freq_his
         case ("avg")
            if (.not. var%to_avg) cycle
            freq = var%freq_avg
         case ("rst")
            if (.not. var%to_rst) cycle
            freq = var%freq_rst
         end select
         
         ! Skip if frequency not defined
         if (freq <= 0) cycle
         
         ! Generate filename
         filename = generate_filename(var%file_prefix, FILE_TYPES(j), freq)
         
         ! For debugging
         print *, "Creating file for var:", trim(var%name), &
                  " prefix='", trim(var%file_prefix), "'", &
                  " type=", trim(FILE_TYPES(j)), &
                  " freq=", freq, &
                  " -> ", trim(filename)
         
         ! Check if file already exists in our list
         file_exists = .false.
         if (allocated(open_files)) then
            do file_idx = 1, size(open_files)
               if (trim(open_files(file_idx)%filename) == trim(filename)) then
                  file_exists = .true.
                  exit
               end if
            end do
         end if
         
         ! If file doesn't exist, create it
         if (.not. file_exists) then
            print *, "Creating file: ", trim(filename), " for variable: ", trim(var%name)
            
            ! Create NetCDF file with appropriate calendar settings
            if (present(time_units) .and. present(calendar)) then
               if (nc_create_file(filename, FILE_TYPES(j), freq, time_units, calendar, file_desc) == 0) then
                  ! Define all applicable variables in this file
                  do i = 1, var_registry%size()
                     var = var_registry%get(i)
                     
                     ! Determine expected prefix for this variable
                     if (trim(var%file_prefix) /= "") then
                        expected_prefix = trim(var%file_prefix)
                     else
                        expected_prefix = trim(get_output_prefix())
                     end if
                     
                     ! Check if this variable belongs in this file
                     if (index(filename, trim(expected_prefix)) /= 1) then
                        cycle  ! Not a file for this variable
                     end if
                     
                     ! Check if this variable should be written to this file type
                     select case (trim(FILE_TYPES(j)))
                     case ("his")
                        if (.not. var%to_his) cycle
                     case ("avg")
                        if (.not. var%to_avg) cycle
                     case ("rst")
                        if (.not. var%to_rst) cycle
                     end select
                     
                     ! Define the variable in the file
                     status = nc_define_variable_in_file(file_desc, var, FILE_TYPES(j))
                     if (status /= 0) then
                        print *, "Warning: Failed to define variable ", trim(var%name), " in file ", trim(filename)
                     end if
                  end do
                  
                  status = nc_end_definition(file_desc)
                  call add_to_open_files(file_desc)
                  num_files = num_files + 1
               end if
            else if (present(time_units)) then
               if (nc_create_file(filename, FILE_TYPES(j), freq, time_units, file_desc=file_desc) == 0) then
                  ! Define all applicable variables in this file
                  do i = 1, var_registry%size()
                     var = var_registry%get(i)
                     
                     ! Determine expected prefix for this variable
                     if (trim(var%file_prefix) /= "") then
                        expected_prefix = trim(var%file_prefix)
                     else
                        expected_prefix = trim(get_output_prefix())
                     end if
                     
                     ! Check if this variable belongs in this file
                     if (index(filename, trim(expected_prefix)) /= 1) then
                        cycle  ! Not a file for this variable
                     end if
                     
                     ! Check if this variable should be written to this file type
                     select case (trim(FILE_TYPES(j)))
                     case ("his")
                        if (.not. var%to_his) cycle
                     case ("avg")
                        if (.not. var%to_avg) cycle
                     case ("rst")
                        if (.not. var%to_rst) cycle
                     end select
                     
                     ! Define the variable in the file
                     status = nc_define_variable_in_file(file_desc, var, FILE_TYPES(j))
                     if (status /= 0) then
                        print *, "Warning: Failed to define variable ", trim(var%name), " in file ", trim(filename)
                     end if
                  end do
                  
                  status = nc_end_definition(file_desc)
                  call add_to_open_files(file_desc)
                  num_files = num_files + 1
               end if
            else
               if (nc_create_file(filename, FILE_TYPES(j), freq, file_desc=file_desc) == 0) then
                  ! Define all applicable variables in this file
                  do i = 1, var_registry%size()
                     var = var_registry%get(i)
                     
                     ! Determine expected prefix for this variable
                     if (trim(var%file_prefix) /= "") then
                        expected_prefix = trim(var%file_prefix)
                     else
                        expected_prefix = trim(get_output_prefix())
                     end if
                     
                     ! Check if this variable belongs in this file
                     if (index(filename, trim(expected_prefix)) /= 1) then
                        cycle  ! Not a file for this variable
                     end if
                     
                     ! Check if this variable should be written to this file type
                     select case (trim(FILE_TYPES(j)))
                     case ("his")
                        if (.not. var%to_his) cycle
                     case ("avg")
                        if (.not. var%to_avg) cycle
                     case ("rst")
                        if (.not. var%to_rst) cycle
                     end select
                     
                     ! Define the variable in the file
                     status = nc_define_variable_in_file(file_desc, var, FILE_TYPES(j))
                     if (status /= 0) then
                        print *, "Warning: Failed to define variable ", trim(var%name), " in file ", trim(filename)
                     end if
                  end do
                  
                  status = nc_end_definition(file_desc)
                  call add_to_open_files(file_desc)
                  num_files = num_files + 1
               end if
            end if
         end if
      end do
   end do
end function create_output_files
   !> Add a file to the registry of open files
   !>
   !> @param[in] file_desc  File descriptor to add
   subroutine add_to_open_files(file_desc)
      type(file_descriptor), intent(in) :: file_desc
      type(file_descriptor), allocatable :: temp(:)
      integer :: n
      
      ! Add file to the list
      if (.not. allocated(open_files)) then
         allocate(open_files(1))
         open_files(1) = file_desc
      else
         ! Resize the array to add one more element
         n = size(open_files)
         allocate(temp(n + 1))
         temp(1:n) = open_files
         call move_alloc(temp, open_files)
         
         ! Add new file
         open_files(n + 1) = file_desc
      end if
   end subroutine add_to_open_files

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
      type(io_variable) :: var
      
      status = -1
      
      ! Find the variable in the registry
      var_idx = var_registry%find(var_name)
      if (var_idx < 0) then
         print *, "Error: Variable not found: ", trim(var_name)
         return
      end if
      
      var = var_registry%get(var_idx)
      
      ! Process all open files
      if (allocated(open_files)) then
         do file_idx = 1, size(open_files)
            ! Determine if this variable should be written to this file
            if (should_write_to_file(var, open_files(file_idx), current_time)) then
               ! Write variable data
               status = nc_write_variable_data(open_files(file_idx), var, current_time)
               
               ! Write time value
               if (status == 0) then
                  status = nc_write_time(open_files(file_idx), current_time)
               end if
            end if
         end do
      end if
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
   type(io_variable) :: var
   logical :: final_step, any_written, is_avg_write_time
   
   status = 0
   
   ! Check if this is the final time step
   final_step = .false.
   if (present(is_final_step)) then
      final_step = is_final_step
   end if
   
   ! Create output files if they don't exist yet
   if (.not. allocated(open_files)) then
      if (create_output_files(stored_time_units, stored_calendar) == 0) then
         print *, "Warning: No output files created"
         status = -1
         return
      end if
   end if
   
   ! First, accumulate averages for all variables
   do var_idx = 1, var_registry%size()
      var = var_registry%get(var_idx)
      
      ! Only accumulate for variables that need averages
      if (var%to_avg) then
                 call accumulate_avg(var)
      end if
   end do
   
   ! Process all open files
   if (allocated(open_files)) then
      do file_idx = 1, size(open_files)
         any_written = .false.
         
         ! For restart files, force writing on final step
         if (trim(open_files(file_idx)%type) == "rst" .and. final_step) then
            any_written = .true.
         end if
         
         ! Check if it's time to write averages
         is_avg_write_time = .false.
         if (trim(open_files(file_idx)%type) == "avg") then
            is_avg_write_time = abs(mod(current_time, open_files(file_idx)%freq)) < TOLERANCE
         end if
         
         ! Process all variables for this file
         do var_idx = 1, var_registry%size()
            var = var_registry%get(var_idx)
            
            ! Determine if this variable should be written to this file
            if (should_write_to_file(var, open_files(file_idx), current_time) .or. &
               (final_step .and. trim(open_files(file_idx)%type) == "rst" .and. var%to_rst .and. &
                belongs_to_file(var, open_files(file_idx)))) then
               
               ! Write variable data
               if (trim(open_files(file_idx)%type) == "avg" .and. is_avg_write_time) then
                   ! Écrire la moyenne
                   if (write_variable_avg(var, open_files(file_idx)%backend_id,     &
                                        get_varid_for_variable(var, open_files(file_idx)%type, &
                                        open_files(file_idx)%backend_id),&
                                        open_files(file_idx)%time_index) == 0) then
                       any_written = .true.
                       
                       ! Réinitialiser la moyenne après écriture
                       call reset_avg(var)
                   end if
               else
                  ! Write normal data
                  if (nc_write_variable_data(open_files(file_idx), var, current_time) == 0) then
                     any_written = .true.
                  end if
               end if
            end if
         end do
         
         ! Write time value if any variables were written
         if (any_written) then
            status = nc_write_time(open_files(file_idx), current_time)
         end if
      end do
   end if
end function write_all_data

   !> Determine if a variable should be written to a file at the current time
   !>
   !> @param[in] var          Variable to check
   !> @param[in] file_desc    File descriptor
   !> @param[in] current_time Current model time
   !> @return    True if variable should be written to this file
!> Determine if a variable should be written to a file at the current time
!>
!> This function checks if a variable should be written to a specific file
!> at the current model time, based on file prefix, output configuration,
!> and timing settings.
!>
!> @param[in] var          Variable to check
!> @param[in] file_desc    File descriptor
!> @param[in] current_time Current model time
!> @return    True if variable should be written to this file
function should_write_to_file(var, file_desc, current_time) result(should_write)
   type(io_variable), intent(in) :: var
   type(file_descriptor), intent(in) :: file_desc
   real, intent(in) :: current_time
   logical :: should_write
   
   real :: freq
   character(len=128) :: expected_prefix
   
   should_write = .false.
   


   ! Determine expected prefix for this variable
   if (trim(var%file_prefix) /= "") then
      expected_prefix = trim(var%file_prefix)
   else
      expected_prefix = trim(get_output_prefix())
   end if
   
   ! Check if this variable belongs in this file
   if (index(file_desc%filename, trim(expected_prefix)) /= 1) then
      return  ! Not a file for this variable
   end if
   
   ! Check if variable should be written to this file type
   select case (trim(file_desc%type))
   case ("his")
      if (.not. var%to_his) return
      freq = var%freq_his
   case ("avg")
      if (.not. var%to_avg) return
      freq = var%freq_avg
   case ("rst")
      if (.not. var%to_rst) return
      freq = var%freq_rst
   case default
      return
   end select


 
   
   ! Check frequency
   if (freq <= 0.0) return



   
   ! Determine if we should write at this time step
   if (abs(current_time) < TOLERANCE) then
      ! First time step
      if (trim(file_desc%type) == "his") then
         ! For "his" files - always write at time 0
         should_write = .true.
      else
         ! For "avg" and "rst" files - don't write at time 0
         should_write = .false.
      end if
   else
  if (trim(file_desc%type) == "rst" .and. abs(current_time - file_desc%freq) < TOLERANCE) then
      ! Spécial pour les fichiers de redémarrage - vérifiez si le temps actuel est exactement égal à la fréquence
      should_write = .true.
   else
      should_write = abs(mod(current_time, freq)) < TOLERANCE
   end if
end if


 ! Pour déboguer le problème spécifique
   if (trim(file_desc%filename) == "special_rst_7200s.nc" .and. &
       abs(current_time - 18000.0) < 10.0) then
      print *, "Checking write for variable ", trim(var%name), " to file ", &
               trim(file_desc%filename), " at time ", current_time
      print *, "  Prefix match: ", index(file_desc%filename, trim(expected_prefix)) == 1
      print *, "  File type: ", trim(file_desc%type), ", var.to_rst = ", var%to_rst
      print *, "  Frequency: ", freq, ", mod(time,freq) = ", mod(current_time, freq)
      print *, should_write
      print *, abs(current_time - file_desc%freq)
   end if

end function should_write_to_file

!> Check if a variable belongs to a file based on prefix
!>
!> @param[in] var        Variable to check
!> @param[in] file_desc  File descriptor
!> @return    True if the variable belongs to this file
function belongs_to_file(var, file_desc) result(belongs)
   type(io_variable), intent(in) :: var
   type(file_descriptor), intent(in) :: file_desc
   logical :: belongs
   character(len=128) :: expected_prefix
   
   ! Determine expected prefix for this variable
   if (trim(var%file_prefix) /= "") then
      expected_prefix = trim(var%file_prefix)
   else
      expected_prefix = trim(get_output_prefix())
   end if
   
   ! Check if this file matches the expected prefix
   belongs = (index(file_desc%filename, trim(expected_prefix)) == 1)
end function belongs_to_file

end module io_manager