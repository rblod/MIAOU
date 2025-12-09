!===============================================================================
!> @file mpi_param.F90
!>
!> @brief MPI parameters and domain decomposition variables (CROCO-style)
!>
!> This module defines the MPI decomposition parameters similar to CROCO.
!> All variables are public and set during MPI initialization.
!>
!> Compile with -DMPI to enable MPI support.
!>
!> @author MIAOU Team
!> @date 2025-12
!===============================================================================
module mpi_param
   implicit none
   
#ifdef MPI
   !---------------------------------------------------------------------------
   ! Domain decomposition parameters (set at compile time or runtime)
   !---------------------------------------------------------------------------
   
   !> Number of processes in XI (x) direction
   integer, parameter :: NP_XI = 2
   
   !> Number of processes in ETA (y) direction  
   integer, parameter :: NP_ETA = 2
   
   !> Total number of MPI processes (must equal NP_XI * NP_ETA)
   integer, parameter :: NNODES = NP_XI * NP_ETA

   !---------------------------------------------------------------------------
   ! Parallel I/O mode
   !---------------------------------------------------------------------------
   
   !> I/O mode constants
   integer, parameter :: IO_MODE_SEPARATE = 0   !< Separate file per process
   integer, parameter :: IO_MODE_PARALLEL = 1   !< Single file, parallel NetCDF-4
   
   !> Current I/O mode
   !> - Default: IO_MODE_SEPARATE (works with any MPI + NetCDF)
   !> - IO_MODE_PARALLEL requires -DNC4PAR and NetCDF built with parallel HDF5
#ifdef NC4PAR
   integer, parameter :: mpi_io_mode = IO_MODE_PARALLEL
#else
   integer, parameter :: mpi_io_mode = IO_MODE_SEPARATE
#endif

   !---------------------------------------------------------------------------
   ! Global domain size (interior points, excluding ghost cells)
   !---------------------------------------------------------------------------
   
   !> Global size in XI direction
   integer, parameter :: LLm = 40
   
   !> Global size in ETA direction
   integer, parameter :: MMm = 40
   
   !> Number of vertical levels
   integer, parameter :: N = 5

   !---------------------------------------------------------------------------
   ! Runtime MPI variables (set by mpi_setup)
   !---------------------------------------------------------------------------
   
   !> This process rank (0 to NNODES-1)
   integer, save :: mynode = 0
   
   !> Process coordinate in XI direction (0 to NP_XI-1)
   integer, save :: ii = 0
   
   !> Process coordinate in ETA direction (0 to NP_ETA-1)
   integer, save :: jj = 0
   
   !> True if this is the master process (mynode == 0)
   logical, save :: is_master = .true.

   !---------------------------------------------------------------------------
   ! Neighbor process ranks
   !---------------------------------------------------------------------------
   integer, save :: p_W = -1   !< West neighbor
   integer, save :: p_E = -1   !< East neighbor
   integer, save :: p_S = -1   !< South neighbor
   integer, save :: p_N = -1   !< North neighbor
   integer, save :: p_SW = -1  !< Southwest corner neighbor
   integer, save :: p_SE = -1  !< Southeast corner neighbor
   integer, save :: p_NW = -1  !< Northwest corner neighbor
   integer, save :: p_NE = -1  !< Northeast corner neighbor

   !---------------------------------------------------------------------------
   ! Boundary flags (true if this process has an internal boundary)
   !---------------------------------------------------------------------------
   logical, save :: WEST_INTER  = .false.
   logical, save :: EAST_INTER  = .false.
   logical, save :: SOUTH_INTER = .false.
   logical, save :: NORTH_INTER = .false.

   !---------------------------------------------------------------------------
   ! Local domain bounds in global coordinates
   !---------------------------------------------------------------------------
   integer, save :: iminmpi = 1   !< Local i minimum in global grid
   integer, save :: imaxmpi = 1   !< Local i maximum in global grid
   integer, save :: jminmpi = 1   !< Local j minimum in global grid
   integer, save :: jmaxmpi = 1   !< Local j maximum in global grid

   !---------------------------------------------------------------------------
   ! Local domain dimensions
   !---------------------------------------------------------------------------
   integer, save :: Lmmpi = 1    !< Local size in XI (= imaxmpi - iminmpi + 1)
   integer, save :: Mmmpi = 1    !< Local size in ETA (= jmaxmpi - jminmpi + 1)
   integer, save :: Lm = 1       !< Alias for Lmmpi (used in allocations)
   integer, save :: Mm = 1       !< Alias for Mmmpi (used in allocations)

#else
   !---------------------------------------------------------------------------
   ! Serial mode: simple defaults
   !---------------------------------------------------------------------------
   integer, parameter :: mynode = 0
   logical, parameter :: is_master = .true.
   integer, parameter :: NNODES = 1
   integer, parameter :: NP_XI = 1
   integer, parameter :: NP_ETA = 1
   
   ! In serial mode, these are set by the application
   integer, save :: Lm = 40
   integer, save :: Mm = 40
   integer, parameter :: N = 5
   
#endif

end module mpi_param
