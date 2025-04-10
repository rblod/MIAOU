module namelist_output
   implicit none
   private
   ! Ajout des fréquences globales et des flags globaux
   public :: his_prefix, avg_prefix, rst_prefix, var_output_config, dyn_vars
   public :: global_freq_his, global_freq_avg, global_freq_rst
   public :: global_to_his, global_to_avg, global_to_rst
   public :: read_output_namelist

   ! Constants for error handling
   integer, parameter :: SUCCESS = 0
   integer, parameter :: FILE_NOT_FOUND = -1
   integer, parameter :: NAMELIST_ERROR = -2

   !> @var his_prefix
   !> Prefix for history output files
   character(len=128) :: his_prefix = "history"

   !> @var avg_prefix
   !> Prefix for time average output files
   character(len=128) :: avg_prefix = "average"

   !> @var rst_prefix
   !> Prefix for restart files
   character(len=128) :: rst_prefix = "restart"

   !> @var global_freq_his
   !> Default frequency for history files (seconds)
   real :: global_freq_his = 3600.0

   !> @var global_freq_avg
   !> Default frequency for average files (seconds)
   real :: global_freq_avg = 7200.0

   !> @var global_freq_rst
   !> Default frequency for restart files (seconds)
   real :: global_freq_rst = 86400.0

   !> @var global_to_his
   !> Default flag for writing to history files
   logical :: global_to_his = .true.

   !> @var global_to_avg
   !> Default flag for writing to average files
   logical :: global_to_avg = .false.

   !> @var global_to_rst
   !> Default flag for writing to restart files
   logical :: global_to_rst = .true.

   !> Configuration type for variable output settings
   !>
   !> This type defines all parameters needed to configure how a variable
   !> is written to different output file types.
   !> @note Used directly in namelist input
   type :: var_output_config
      !> Variable name
      character(len=32)  :: name

      !> Flag to enable/disable history file output
      logical            :: wrt = .false.

      !> Flag to enable/disable average file output
      logical            :: avg = .false.

      !> Flag to enable/disable restart file output
      logical            :: rst = .false.

      !> Custom file prefix for this variable (if not using global prefix)
      character(len=128) :: file_prefix = ""

      !> Output frequency for history files (seconds)
      !> @note Negative value disables output
      real               :: freq_his = -1.0

      !> Output frequency for average files (seconds)
      !> @note Negative value disables output
      real               :: freq_avg = -1.0

      !> Output frequency for restart files (seconds)
      !> @note Negative value disables output
      real               :: freq_rst = -1.0
   end type var_output_config

   !> Array of variable output configurations read from namelist
   type(var_output_config), allocatable :: dyn_vars(:)

   ! Namelist declarations - Add global parameters to the namelist
   namelist /output_global/ his_prefix, avg_prefix, rst_prefix, &
                           global_freq_his, global_freq_avg, global_freq_rst, &
                           global_to_his, global_to_avg, global_to_rst
   namelist /output_dyn/ dyn_vars

contains

   !> Get a free unit number for file opening
   function get_free_unit() result(unit_id)
      integer :: unit_id
      logical :: is_open
      
      ! Start at 10 to avoid standard units
      unit_id = 10
      do
         inquire(unit=unit_id, opened=is_open)
         if (.not. is_open) return
         unit_id = unit_id + 1
      end do
   end function get_free_unit

   !> Handle errors in a centralized way
   subroutine handle_error(error_code, message, iostatus, details)
      integer, intent(in) :: error_code, iostatus
      character(len=*), intent(in) :: message
      character(len=*), intent(in), optional :: details
      
      select case (error_code)
         case (FILE_NOT_FOUND)
            print *, "WARNING: ", trim(message), " [Error code: ", iostatus, "]"
            print *, "Using default values instead."
         case (NAMELIST_ERROR)
            print *, "WARNING: ", trim(message), " [Error code: ", iostatus, "]"
            if (present(details)) print *, "Details: ", trim(details)
      end select
   end subroutine handle_error

   !> Count the number of entries in a namelist
   function count_namelist_entries(unit_id, namelist_name) result(count)
      integer, intent(in) :: unit_id
      character(len=*), intent(in) :: namelist_name
      integer :: count, ios
      character(len=1000) :: line
      logical :: in_namelist
      
      count = 0
      in_namelist = .false.
      
      rewind(unit_id)
      
      ! Scan file to count entries
      do
         read(unit_id, '(A)', iostat=ios) line
         if (ios /= 0) exit
         
         ! Determine if we're in the target namelist
         if (index(line, '&'//trim(namelist_name)) > 0) then
            in_namelist = .true.
         end if
         
         ! Count variable entries (lines with "dyn_vars")
         if (in_namelist .and. index(line, 'dyn_vars') > 0) then
            count = count + 1
         end if
         
         ! Detect end of namelist
         if (in_namelist .and. index(line, '/') > 0) then
            exit
         end if
      end do
      
      rewind(unit_id)
   end function count_namelist_entries

   !> Read output configuration from namelist file
   !>
   !> This subroutine reads both global output settings and variable-specific
   !> output settings from the namelist file 'output_config.nml'.
   !> If the file cannot be read, default values are used.
   !>
   !> @note The namelist format must match the structure defined by the
   !>       namelist declarations in this module.
   subroutine read_output_namelist()
      integer :: ios, i, n_read, unit_id
      character(len=256) :: error_msg
      
      ! Free memory if already allocated
      if (allocated(dyn_vars)) deallocate(dyn_vars)
      
      ! Open namelist file with standardized error handling
      unit_id = get_free_unit()
      open(unit=unit_id, file="output_config.nml", status="old", action="read", iostat=ios)
      
      if (ios /= 0) then
         call handle_error(FILE_NOT_FOUND, "Cannot open output_config.nml", ios)
         ! Allocate empty array and return
         allocate(dyn_vars(0))
         return
      end if
      
      ! Read global configuration parameters
      read(unit_id, nml=output_global, iostat=ios, iomsg=error_msg)
      if (ios /= 0) then
         call handle_error(NAMELIST_ERROR, "Error reading /output_global/", ios, error_msg)
         rewind(unit_id)
      end if
      
      ! First count the number of variables in the namelist file
      ! without allocating memory, using a counting mode read
      n_read = count_namelist_entries(unit_id, "output_dyn")
      
      ! Now allocate with exact size needed
      if (n_read > 0) then
         allocate(dyn_vars(n_read))
         ! Initialize with default values
         do i = 1, n_read
            dyn_vars(i)%name = ""
            dyn_vars(i)%wrt = .false.
            dyn_vars(i)%avg = .false.
            dyn_vars(i)%rst = .false.
            dyn_vars(i)%file_prefix = ""
            dyn_vars(i)%freq_his = -1.0
            dyn_vars(i)%freq_avg = -1.0
            dyn_vars(i)%freq_rst = -1.0
         end do
         
         ! Read the namelist again to fill values
         rewind(unit_id)
         read(unit_id, nml=output_dyn, iostat=ios, iomsg=error_msg)
         if (ios /= 0) then
            call handle_error(NAMELIST_ERROR, "Error reading /output_dyn/", ios, error_msg)
            deallocate(dyn_vars)
            allocate(dyn_vars(0))
         end if
      else
         allocate(dyn_vars(0))
      end if
      
      close(unit_id)
   end subroutine read_output_namelist

end module namelist_output