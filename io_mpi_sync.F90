!===============================================================================
!> @file io_mpi_sync.F90
!>
!> @brief MPI I/O synchronization routines
!>
!> This module provides token-passing synchronization for sequential I/O
!> in MPI mode. This allows multiple processes to write to a single NetCDF
!> file without requiring parallel NetCDF support.
!>
!> The mechanism works as follows:
!> 1. Process 0 writes first (no wait)
!> 2. Process 0 closes file and sends token to process 1
!> 3. Process 1 receives token, opens file, writes, closes, sends to process 2
!> 4. ... and so on until all processes have written
!>
!> This is the default MPI I/O mode (like CROCO without PARALLEL_FILES).
!>
!> Compile options:
!> - MPI only: token passing (this module)
!> - MPI + PARALLEL_FILES: separate files per process
!> - MPI + NC4PAR: parallel NetCDF-4 (single file, parallel writes)
!>
!> @author MIAOU Team
!> @date 2025-12
!===============================================================================
module io_mpi_sync

#ifdef MPI
   use mpi_param, only: mynode, NNODES
#endif

   implicit none
   private

   public :: io_wait_turn
   public :: io_pass_turn
   public :: io_is_sequential_mode

   ! Token tag for MPI communication
   integer, parameter :: IO_TOKEN_TAG = 1001

contains

   !---------------------------------------------------------------------------
   !> @brief Check if we are in sequential I/O mode
   !---------------------------------------------------------------------------
   function io_is_sequential_mode() result(is_seq)
      logical :: is_seq

#if defined(MPI) && !defined(PARALLEL_FILES) && !defined(NC4PAR)
      is_seq = .true.
#else
      is_seq = .false.
#endif
   end function io_is_sequential_mode

   !---------------------------------------------------------------------------
   !> @brief Wait for I/O turn (receive token from previous process)
   !---------------------------------------------------------------------------
   subroutine io_wait_turn()
#if defined(MPI) && !defined(PARALLEL_FILES) && !defined(NC4PAR)
      include 'mpif.h'
      integer :: token, ierr
      integer :: status(MPI_STATUS_SIZE)

      if (mynode > 0) then
         call MPI_Recv(token, 1, MPI_INTEGER, mynode - 1, &
                       IO_TOKEN_TAG, MPI_COMM_WORLD, status, ierr)
      end if
#endif
   end subroutine io_wait_turn

   !---------------------------------------------------------------------------
   !> @brief Pass I/O turn to next process (send token)
   !---------------------------------------------------------------------------
   subroutine io_pass_turn()
#if defined(MPI) && !defined(PARALLEL_FILES) && !defined(NC4PAR)
      include 'mpif.h'
      integer :: token, ierr

      token = 1

      if (mynode < NNODES - 1) then
         call MPI_Send(token, 1, MPI_INTEGER, mynode + 1, &
                       IO_TOKEN_TAG, MPI_COMM_WORLD, ierr)
      end if
#endif
   end subroutine io_pass_turn

end module io_mpi_sync
