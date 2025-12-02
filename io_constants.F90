!===============================================================================
!> @file io_constants.F90
!>
!> Centralized constants for the I/O system
!>
!> This module defines all constants used throughout the I/O system,
!> ensuring consistency and easy maintenance.
!>
!> @author Rachid Benshila
!> @date 2025-04
!===============================================================================
module io_constants
   implicit none
   public

   !---------------------------------------------------------------------------
   ! Numerical tolerances
   !---------------------------------------------------------------------------
   
   !> Tolerance for time comparisons (seconds)
   real, parameter :: IO_TIME_TOLERANCE = 1.0e-5

   !---------------------------------------------------------------------------
   ! String lengths
   !---------------------------------------------------------------------------
   
   !> Maximum length for variable names
   integer, parameter :: IO_VARNAME_LEN = 32
   
   !> Maximum length for long names / descriptions
   integer, parameter :: IO_LONGNAME_LEN = 64
   
   !> Maximum length for units
   integer, parameter :: IO_UNITS_LEN = 32
   
   !> Maximum length for file prefixes
   integer, parameter :: IO_PREFIX_LEN = 128
   
   !> Maximum length for file paths
   integer, parameter :: IO_PATH_LEN = 256
   
   !> Maximum length for file type identifiers ("his", "avg", "rst")
   integer, parameter :: IO_FILETYPE_LEN = 16

   !---------------------------------------------------------------------------
   ! Array size limits
   !---------------------------------------------------------------------------
   
   !> Maximum number of variables that can be registered
   integer, parameter :: IO_MAX_VARS = 100
   
   !> Maximum number of output files
   integer, parameter :: IO_MAX_FILES = 50
   
   !> Maximum number of dimensions per variable
   integer, parameter :: IO_MAX_DIMS = 4
   
   !> Initial allocation size for dynamic arrays
   integer, parameter :: IO_INITIAL_ALLOC = 10
   
   !> Growth factor for dynamic array expansion (percentage)
   integer, parameter :: IO_GROWTH_FACTOR = 50

   !---------------------------------------------------------------------------
   ! File type identifiers
   !---------------------------------------------------------------------------
   
   !> History file type identifier
   character(len=*), parameter :: IO_TYPE_HIS = "his"
   
   !> Average file type identifier
   character(len=*), parameter :: IO_TYPE_AVG = "avg"
   
   !> Restart file type identifier
   character(len=*), parameter :: IO_TYPE_RST = "rst"
   
   !> Array of all file types (for iteration)
   character(len=3), parameter :: IO_FILE_TYPES(3) = ["his", "avg", "rst"]
   
   !> Number of file types
   integer, parameter :: IO_NUM_FILE_TYPES = 3

   !---------------------------------------------------------------------------
   ! Default values
   !---------------------------------------------------------------------------
   
   !> Default output frequency (disabled)
   real, parameter :: IO_FREQ_DISABLED = -1.0
   
   !> Default file prefix
   character(len=*), parameter :: IO_DEFAULT_PREFIX = "output"

end module io_constants
