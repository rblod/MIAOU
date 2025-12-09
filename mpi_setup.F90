!===============================================================================
!> @file mpi_setup.F90
!>
!> @brief MPI initialization and domain decomposition (CROCO-style)
!>
!> Handles MPI initialization and 2D domain decomposition following CROCO's
!> approach. Each process gets a rectangular subdomain of the global grid.
!>
!> @author MIAOU Team
!> @date 2025-12
!===============================================================================
module mpi_setup
   use mpi_param
   implicit none
   private

   public :: mpi_init_decomposition
   public :: mpi_finalize_all
   public :: mpi_barrier_all
   public :: mpi_get_node
   public :: mpi_is_master

contains

#ifdef MPI

   !---------------------------------------------------------------------------
   !> @brief Initialize MPI and setup domain decomposition
   !>
   !> This follows CROCO's MPI_Setup pattern:
   !> 1. Initialize MPI
   !> 2. Check number of processes matches NNODES
   !> 3. Compute 2D process coordinates (ii, jj)
   !> 4. Determine neighbor ranks
   !> 5. Compute local grid bounds
   !>
   !> @param[out] ierr  Error code (0 = success)
   !---------------------------------------------------------------------------
   subroutine mpi_init_decomposition(ierr)
      use mpi
      integer, intent(out) :: ierr

      integer :: nsize
      integer :: chunk_size_X, margin_X
      integer :: chunk_size_E, margin_E
      integer :: i_X, j_E
      integer :: ii_W, ii_E, jj_S, jj_N

      ierr = 0

      ! Initialize MPI
      call MPI_Init(ierr)
      if (ierr /= 0) return

      ! Get number of processes and this process rank
      call MPI_Comm_size(MPI_COMM_WORLD, nsize, ierr)
      call MPI_Comm_rank(MPI_COMM_WORLD, mynode, ierr)

      ! Check that number of processes matches expected
      if (nsize /= NNODES) then
         if (mynode == 0) then
            print '(/1x,A,I5,1x,A,I5,A/)', &
               'ERROR in MPI_Setup: number of MPI processes should be', &
               NNODES, 'instead of', nsize, '.'
         end if
         ierr = 99
         return
      end if

      ! Set master flag
      is_master = (mynode == 0)

      !------------------------------------------------------------------------
      ! Compute 2D process coordinates
      ! Process layout: row-major order
      !   mynode = ii + NP_XI * jj
      !------------------------------------------------------------------------
      ii = mod(mynode, NP_XI)
      jj = mynode / NP_XI

      !------------------------------------------------------------------------
      ! Determine boundary flags
      !------------------------------------------------------------------------
      if (NP_XI == 1) then
         WEST_INTER = .false.
         EAST_INTER = .false.
      else
         WEST_INTER = (ii /= 0)
         EAST_INTER = (ii /= NP_XI - 1)
      end if

      if (NP_ETA == 1) then
         SOUTH_INTER = .false.
         NORTH_INTER = .false.
      else
         SOUTH_INTER = (jj /= 0)
         NORTH_INTER = (jj /= NP_ETA - 1)
      end if

      !------------------------------------------------------------------------
      ! Compute neighbor ranks (with periodic wrapping for internal use)
      !------------------------------------------------------------------------
      ii_W = mod(ii - 1 + NP_XI, NP_XI)
      ii_E = mod(ii + 1, NP_XI)
      jj_S = mod(jj - 1 + NP_ETA, NP_ETA)
      jj_N = mod(jj + 1, NP_ETA)

      ! Cardinal neighbors
      p_W = ii_W + NP_XI * jj
      p_E = ii_E + NP_XI * jj
      p_S = ii + NP_XI * jj_S
      p_N = ii + NP_XI * jj_N

      ! Corner neighbors
      p_SW = ii_W + NP_XI * jj_S
      p_SE = ii_E + NP_XI * jj_S
      p_NW = ii_W + NP_XI * jj_N
      p_NE = ii_E + NP_XI * jj_N

      !------------------------------------------------------------------------
      ! Compute local grid bounds (CROCO-style decomposition)
      !------------------------------------------------------------------------
      j_E = mynode / NP_XI
      i_X = mynode - j_E * NP_XI

      ! Chunk sizes with margin distribution
      chunk_size_X = (LLm + NP_XI - 1) / NP_XI
      margin_X = (NP_XI * chunk_size_X - LLm) / 2

      chunk_size_E = (MMm + NP_ETA - 1) / NP_ETA
      margin_E = (NP_ETA * chunk_size_E - MMm) / 2

      ! Local bounds in global coordinates (1-based)
      iminmpi = 1 + i_X * chunk_size_X - margin_X
      imaxmpi = iminmpi + chunk_size_X - 1
      iminmpi = max(iminmpi, 1)
      imaxmpi = min(imaxmpi, LLm)

      jminmpi = 1 + j_E * chunk_size_E - margin_E
      jmaxmpi = jminmpi + chunk_size_E - 1
      jminmpi = max(jminmpi, 1)
      jmaxmpi = min(jmaxmpi, MMm)

      ! Local dimensions
      Lmmpi = imaxmpi - iminmpi + 1
      Mmmpi = jmaxmpi - jminmpi + 1
      Lm = Lmmpi
      Mm = Mmmpi

      !------------------------------------------------------------------------
      ! Print decomposition info
      !------------------------------------------------------------------------
      if (is_master) then
         print '(/A)', '================================================'
         print '(A)',  ' MPI Domain Decomposition'
         print '(A)',  '================================================'
         print '(A,I4,A,I4,A,I4)', ' Global grid: ', LLm, ' x ', MMm, ' x ', N
         print '(A,I4,A,I4,A,I4)', ' MPI layout:  ', NP_XI, ' x ', NP_ETA, &
                                  ' = ', NNODES
         print '(A)', '================================================'
      end if

      call MPI_Barrier(MPI_COMM_WORLD, ierr)

      ! Each process prints its info
      print '(A,I3,A,I2,A,I2,A,I4,A,I4,A,I4,A,I4,A,I4,A,I4)', &
         ' Process ', mynode, ' (', ii, ',', jj, '): i=[', &
         iminmpi, ':', imaxmpi, '] j=[', jminmpi, ':', jmaxmpi, &
         '] local=', Lm, 'x', Mm

      call MPI_Barrier(MPI_COMM_WORLD, ierr)

      if (is_master) then
         print '(A/)', '================================================'
      end if

   end subroutine mpi_init_decomposition

   !---------------------------------------------------------------------------
   !> @brief Finalize MPI
   !---------------------------------------------------------------------------
   subroutine mpi_finalize_all()
      use mpi
      integer :: ierr

      call MPI_Barrier(MPI_COMM_WORLD, ierr)
      call MPI_Finalize(ierr)

   end subroutine mpi_finalize_all

   !---------------------------------------------------------------------------
   !> @brief MPI Barrier wrapper
   !---------------------------------------------------------------------------
   subroutine mpi_barrier_all()
      use mpi
      integer :: ierr

      call MPI_Barrier(MPI_COMM_WORLD, ierr)

   end subroutine mpi_barrier_all

   !---------------------------------------------------------------------------
   !> @brief Get current process rank
   !---------------------------------------------------------------------------
   function mpi_get_node() result(node)
      integer :: node
      node = mynode
   end function mpi_get_node

   !---------------------------------------------------------------------------
   !> @brief Check if current process is master
   !---------------------------------------------------------------------------
   function mpi_is_master() result(is_master_proc)
      logical :: is_master_proc
      is_master_proc = is_master
   end function mpi_is_master

#else
   !---------------------------------------------------------------------------
   ! Serial stubs
   !---------------------------------------------------------------------------
   
   subroutine mpi_init_decomposition(ierr)
      integer, intent(out) :: ierr
      ierr = 0
   end subroutine mpi_init_decomposition

   subroutine mpi_finalize_all()
   end subroutine mpi_finalize_all

   subroutine mpi_barrier_all()
   end subroutine mpi_barrier_all

   function mpi_get_node() result(node)
      integer :: node
      node = 0
   end function mpi_get_node

   function mpi_is_master() result(is_master_proc)
      logical :: is_master_proc
      is_master_proc = .true.
   end function mpi_is_master

#endif

end module mpi_setup
