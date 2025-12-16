!===============================================================================
!> @file io_backend.F90
!> @brief Backend identification and capability query system
!>
!> @details
!> This module provides a simple, pragmatic approach to multi-backend I/O:
!> - Integer IDs to identify backends (no complex polymorphism)
!> - Simple functions to query backend capabilities
!> - Easy dispatch via select case in calling code
!>
!> Supported backends:
!> - BACKEND_NETCDF: NetCDF/HDF5 (read/write, always available)
!> - BACKEND_XIOS: XIOS I/O server (write only, async, requires USE_XIOS)
!> - BACKEND_ADIOS2: ADIOS2 (read/write, staging, requires USE_ADIOS2)
!>
!> Typical usage pattern:
!> - History/averages: XIOS (async, high performance)
!> - Restart files: NetCDF (reliable, must be readable)
!> - Input files: NetCDF (forcing, initial conditions)
!>
!> @author MIAOU Development Team
!> @version 6.0.0
!> @date December 2025
!===============================================================================
module io_backend
   implicit none
   private

   !============================================================================
   ! Public API
   !============================================================================
   
   ! Backend IDs
   public :: BACKEND_NETCDF, BACKEND_XIOS, BACKEND_ADIOS2
   
   ! Query functions
   public :: backend_from_string
   public :: backend_to_string
   public :: backend_is_available
   
   ! Capability queries - "Can this backend do X?"
   public :: backend_can_read
   public :: backend_can_write
   public :: backend_supports_parallel_io
   public :: backend_supports_compression
   public :: backend_supports_streaming
   public :: backend_supports_staging
   
   ! Responsibility queries - "Does the backend manage X internally?"
   public :: backend_manages_config
   public :: backend_manages_timing
   public :: backend_manages_operations
   public :: backend_supports_steps
   public :: backend_needs_collective_calls
   
   ! Utility
   public :: backend_print_info

   !============================================================================
   ! Backend identifiers
   !============================================================================
   
   !> @name Backend IDs
   !> Integer identifiers for each supported backend
   !> @{
   integer, parameter :: BACKEND_NETCDF = 1   !< NetCDF/HDF5 backend
   integer, parameter :: BACKEND_XIOS   = 2   !< XIOS I/O server backend
   integer, parameter :: BACKEND_ADIOS2 = 3   !< ADIOS2 backend
   !> @}
   
   !> Number of supported backends
   integer, parameter :: NUM_BACKENDS = 3
   
   !> Backend names (for config parsing and display)
   character(len=8), parameter :: BACKEND_NAMES(NUM_BACKENDS) = &
      (/ 'netcdf  ', 'xios    ', 'adios2  ' /)

contains

   !============================================================================
   ! Backend identification
   !============================================================================

   !---------------------------------------------------------------------------
   !> @brief Convert backend name string to ID
   !> @param[in] name Backend name (case-insensitive): 'netcdf', 'xios', 'adios2'
   !> @return Backend ID, or BACKEND_NETCDF if unknown
   !---------------------------------------------------------------------------
   function backend_from_string(name) result(id)
      character(len=*), intent(in) :: name
      integer :: id
      
      character(len=32) :: name_lower
      integer :: i
      
      ! Convert to lowercase for comparison
      name_lower = adjustl(name)
      do i = 1, len_trim(name_lower)
         if (name_lower(i:i) >= 'A' .and. name_lower(i:i) <= 'Z') then
            name_lower(i:i) = char(ichar(name_lower(i:i)) + 32)
         end if
      end do
      
      select case (trim(name_lower))
      case ('netcdf', 'nc', 'hdf5')
         id = BACKEND_NETCDF
      case ('xios')
         id = BACKEND_XIOS
      case ('adios2', 'adios', 'bp5', 'bp')
         id = BACKEND_ADIOS2
      case default
         ! Default to NetCDF for unknown backends
         id = BACKEND_NETCDF
      end select
      
   end function backend_from_string

   !---------------------------------------------------------------------------
   !> @brief Convert backend ID to name string
   !> @param[in] id Backend ID
   !> @return Backend name string
   !---------------------------------------------------------------------------
   function backend_to_string(id) result(name)
      integer, intent(in) :: id
      character(len=8) :: name
      
      if (id >= 1 .and. id <= NUM_BACKENDS) then
         name = BACKEND_NAMES(id)
      else
         name = 'unknown '
      end if
      
   end function backend_to_string

   !---------------------------------------------------------------------------
   !> @brief Check if a backend is available (compiled in)
   !> @param[in] id Backend ID
   !> @return .true. if backend is available
   !---------------------------------------------------------------------------
   logical function backend_is_available(id)
      integer, intent(in) :: id
      
      select case (id)
      case (BACKEND_NETCDF)
         ! NetCDF is always available (required dependency)
         backend_is_available = .true.
         
      case (BACKEND_XIOS)
#ifdef USE_XIOS
         backend_is_available = .true.
#else
         backend_is_available = .false.
#endif
         
      case (BACKEND_ADIOS2)
#ifdef USE_ADIOS2
         backend_is_available = .true.
#else
         backend_is_available = .false.
#endif
         
      case default
         backend_is_available = .false.
      end select
      
   end function backend_is_available

   !============================================================================
   ! Capability queries - "Can this backend do X?"
   !============================================================================

   !---------------------------------------------------------------------------
   !> @brief Check if backend can read files
   !> @param[in] id Backend ID
   !> @return .true. if backend supports reading
   !> @note XIOS is write-only (or very limited read)
   !---------------------------------------------------------------------------
   logical function backend_can_read(id)
      integer, intent(in) :: id
      
      select case (id)
      case (BACKEND_NETCDF)
         backend_can_read = .true.
      case (BACKEND_XIOS)
         backend_can_read = .false.   ! XIOS is write-only
      case (BACKEND_ADIOS2)
         backend_can_read = .true.
      case default
         backend_can_read = .false.
      end select
      
   end function backend_can_read

   !---------------------------------------------------------------------------
   !> @brief Check if backend can write files
   !> @param[in] id Backend ID
   !> @return .true. if backend supports writing
   !---------------------------------------------------------------------------
   logical function backend_can_write(id)
      integer, intent(in) :: id
      
      select case (id)
      case (BACKEND_NETCDF, BACKEND_XIOS, BACKEND_ADIOS2)
         backend_can_write = .true.
      case default
         backend_can_write = .false.
      end select
      
   end function backend_can_write

   !---------------------------------------------------------------------------
   !> @brief Check if backend supports native parallel I/O
   !> @param[in] id Backend ID
   !> @return .true. if backend has native parallel support
   !> @note NetCDF requires NC4PAR compilation for true parallel I/O
   !---------------------------------------------------------------------------
   logical function backend_supports_parallel_io(id)
      integer, intent(in) :: id
      
      select case (id)
      case (BACKEND_NETCDF)
         ! NetCDF parallel support depends on compilation
#ifdef NC4PAR
         backend_supports_parallel_io = .true.
#else
         backend_supports_parallel_io = .false.  ! Limited (sequential or parallel files)
#endif
      case (BACKEND_XIOS)
         backend_supports_parallel_io = .true.   ! Native via I/O servers
      case (BACKEND_ADIOS2)
         backend_supports_parallel_io = .true.   ! Native (MPI-IO, BP5)
      case default
         backend_supports_parallel_io = .false.
      end select
      
   end function backend_supports_parallel_io

   !---------------------------------------------------------------------------
   !> @brief Check if backend supports compression
   !> @param[in] id Backend ID
   !> @return .true. if backend supports data compression
   !---------------------------------------------------------------------------
   logical function backend_supports_compression(id)
      integer, intent(in) :: id
      
      select case (id)
      case (BACKEND_NETCDF)
         backend_supports_compression = .true.   ! Via HDF5 filters (deflate, etc.)
      case (BACKEND_XIOS)
         backend_supports_compression = .true.   ! Declarative in XML
      case (BACKEND_ADIOS2)
         backend_supports_compression = .true.   ! BP5 operators
      case default
         backend_supports_compression = .false.
      end select
      
   end function backend_supports_compression

   !---------------------------------------------------------------------------
   !> @brief Check if backend supports real-time streaming
   !> @param[in] id Backend ID
   !> @return .true. if backend supports data streaming (SST, DataMan)
   !---------------------------------------------------------------------------
   logical function backend_supports_streaming(id)
      integer, intent(in) :: id
      
      select case (id)
      case (BACKEND_NETCDF)
         backend_supports_streaming = .false.
      case (BACKEND_XIOS)
         backend_supports_streaming = .true.    ! Real-time via servers
      case (BACKEND_ADIOS2)
         backend_supports_streaming = .true.    ! SST, DataMan engines
      case default
         backend_supports_streaming = .false.
      end select
      
   end function backend_supports_streaming

   !---------------------------------------------------------------------------
   !> @brief Check if backend supports memory staging
   !> @param[in] id Backend ID
   !> @return .true. if backend can buffer/stage data in memory
   !---------------------------------------------------------------------------
   logical function backend_supports_staging(id)
      integer, intent(in) :: id
      
      select case (id)
      case (BACKEND_NETCDF)
         backend_supports_staging = .false.
      case (BACKEND_XIOS)
         backend_supports_staging = .true.      ! Async buffering
      case (BACKEND_ADIOS2)
         backend_supports_staging = .true.      ! BP5 staging
      case default
         backend_supports_staging = .false.
      end select
      
   end function backend_supports_staging

   !============================================================================
   ! Responsibility queries - "Does the backend manage X internally?"
   ! These determine how MIAOU should interact with the backend
   !============================================================================

   !---------------------------------------------------------------------------
   !> @brief Check if backend manages its own configuration
   !> @param[in] id Backend ID
   !> @return .true. if backend uses external config (XML files)
   !> @note If true, MIAOU namelist config may be ignored for this backend
   !---------------------------------------------------------------------------
   logical function backend_manages_config(id)
      integer, intent(in) :: id
      
      select case (id)
      case (BACKEND_NETCDF)
         backend_manages_config = .false.       ! MIAOU controls everything
      case (BACKEND_XIOS)
         backend_manages_config = .true.        ! iodef.xml controls output
      case (BACKEND_ADIOS2)
         backend_manages_config = .true.        ! adios2.xml (partial)
      case default
         backend_manages_config = .false.
      end select
      
   end function backend_manages_config

   !---------------------------------------------------------------------------
   !> @brief Check if backend manages its own timing
   !> @param[in] id Backend ID
   !> @return .true. if backend decides when to actually write
   !> @note If true, MIAOU sends data; backend decides when to flush to disk
   !---------------------------------------------------------------------------
   logical function backend_manages_timing(id)
      integer, intent(in) :: id
      
      select case (id)
      case (BACKEND_NETCDF)
         backend_manages_timing = .false.       ! MIAOU controls write timing
      case (BACKEND_XIOS)
         backend_manages_timing = .true.        ! XIOS controls async write
      case (BACKEND_ADIOS2)
         backend_manages_timing = .false.       ! MIAOU calls begin_step/end_step
      case default
         backend_manages_timing = .false.
      end select
      
   end function backend_manages_timing

   !---------------------------------------------------------------------------
   !> @brief Check if backend manages its own operations (avg, compression)
   !> @param[in] id Backend ID
   !> @return .true. if backend handles averaging/compression internally
   !---------------------------------------------------------------------------
   logical function backend_manages_operations(id)
      integer, intent(in) :: id
      
      select case (id)
      case (BACKEND_NETCDF)
         backend_manages_operations = .false.   ! MIAOU does averaging
      case (BACKEND_XIOS)
         backend_manages_operations = .true.    ! XIOS does temporal operations
      case (BACKEND_ADIOS2)
         backend_manages_operations = .true.    ! BP5 operators (partial)
      case default
         backend_manages_operations = .false.
      end select
      
   end function backend_manages_operations

   !---------------------------------------------------------------------------
   !> @brief Check if backend has integrated timestep concept
   !> @param[in] id Backend ID
   !> @return .true. if backend uses begin_step/end_step pattern
   !---------------------------------------------------------------------------
   logical function backend_supports_steps(id)
      integer, intent(in) :: id
      
      select case (id)
      case (BACKEND_NETCDF)
         backend_supports_steps = .false.       ! Just record dimension
      case (BACKEND_XIOS)
         backend_supports_steps = .true.        ! update_calendar_timestep
      case (BACKEND_ADIOS2)
         backend_supports_steps = .true.        ! BeginStep/EndStep
      case default
         backend_supports_steps = .false.
      end select
      
   end function backend_supports_steps

   !---------------------------------------------------------------------------
   !> @brief Check if backend requires collective MPI calls
   !> @param[in] id Backend ID
   !> @return .true. if all MPI ranks must call I/O functions together
   !---------------------------------------------------------------------------
   logical function backend_needs_collective_calls(id)
      integer, intent(in) :: id
      
      select case (id)
      case (BACKEND_NETCDF)
         backend_needs_collective_calls = .false.  ! Can write independently
      case (BACKEND_XIOS)
         backend_needs_collective_calls = .true.   ! All ranks must participate
      case (BACKEND_ADIOS2)
         backend_needs_collective_calls = .true.   ! Collective by design
      case default
         backend_needs_collective_calls = .false.
      end select
      
   end function backend_needs_collective_calls

   !============================================================================
   ! Utility functions
   !============================================================================

   !---------------------------------------------------------------------------
   !> @brief Print backend information and capabilities
   !> @param[in] id Backend ID
   !> @param[in] unit Optional output unit (default: stdout)
   !---------------------------------------------------------------------------
   subroutine backend_print_info(id, unit)
      integer, intent(in) :: id
      integer, intent(in), optional :: unit
      
      integer :: out_unit
      character(len=3) :: yes_no
      
      out_unit = 6  ! stdout
      if (present(unit)) out_unit = unit
      
      write(out_unit, '(A)') '----------------------------------------'
      write(out_unit, '(A,A)') 'Backend: ', trim(backend_to_string(id))
      write(out_unit, '(A,L1)') 'Available: ', backend_is_available(id)
      write(out_unit, '(A)') ''
      write(out_unit, '(A)') 'Capabilities:'
      
      yes_no = merge('Yes', 'No ', backend_can_read(id))
      write(out_unit, '(A,A)') '  Can read:            ', yes_no
      
      yes_no = merge('Yes', 'No ', backend_can_write(id))
      write(out_unit, '(A,A)') '  Can write:           ', yes_no
      
      yes_no = merge('Yes', 'No ', backend_supports_parallel_io(id))
      write(out_unit, '(A,A)') '  Parallel I/O:        ', yes_no
      
      yes_no = merge('Yes', 'No ', backend_supports_compression(id))
      write(out_unit, '(A,A)') '  Compression:         ', yes_no
      
      yes_no = merge('Yes', 'No ', backend_supports_streaming(id))
      write(out_unit, '(A,A)') '  Streaming:           ', yes_no
      
      yes_no = merge('Yes', 'No ', backend_supports_staging(id))
      write(out_unit, '(A,A)') '  Staging:             ', yes_no
      
      write(out_unit, '(A)') ''
      write(out_unit, '(A)') 'Responsibilities (backend manages internally):'
      
      yes_no = merge('Yes', 'No ', backend_manages_config(id))
      write(out_unit, '(A,A)') '  Config (XML):        ', yes_no
      
      yes_no = merge('Yes', 'No ', backend_manages_timing(id))
      write(out_unit, '(A,A)') '  Write timing:        ', yes_no
      
      yes_no = merge('Yes', 'No ', backend_manages_operations(id))
      write(out_unit, '(A,A)') '  Operations (avg):    ', yes_no
      
      yes_no = merge('Yes', 'No ', backend_supports_steps(id))
      write(out_unit, '(A,A)') '  Step concept:        ', yes_no
      
      yes_no = merge('Yes', 'No ', backend_needs_collective_calls(id))
      write(out_unit, '(A,A)') '  Collective calls:    ', yes_no
      
      write(out_unit, '(A)') '----------------------------------------'
      
   end subroutine backend_print_info

end module io_backend
