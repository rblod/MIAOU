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
   
   !> Maximum length for file type identifiers
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
   ! Default values
   !---------------------------------------------------------------------------
   
   !> Default output frequency (disabled)
   real, parameter :: IO_FREQ_DISABLED = -1.0
   
   !> Default file prefix
   character(len=*), parameter :: IO_DEFAULT_PREFIX = "output"

   !---------------------------------------------------------------------------
   ! Compression settings (NetCDF-4) - runtime configurable
   !---------------------------------------------------------------------------
   
   !> Enable/disable compression (set by io_config from namelist)
   logical, save :: io_compression_enabled = .true.
   
   !> Compression level 0-9 (set by io_config from namelist)
   integer, save :: io_compression_level = 4

   !---------------------------------------------------------------------------
   ! Flush and verbosity settings - runtime configurable
   !---------------------------------------------------------------------------
   
   !> Flush frequency: sync files every N writes (0 = disabled)
   integer, save :: io_flush_freq = 0
   
   !> Verbosity level: 0=quiet, 1=normal, 2=debug
   integer, save :: io_verbose = 1
   
   !> Verbosity level constants
   integer, parameter :: IO_QUIET = 0
   integer, parameter :: IO_NORMAL = 1
   integer, parameter :: IO_DEBUG = 2

   !---------------------------------------------------------------------------
   ! Fill value
   !---------------------------------------------------------------------------
   
   !> Default fill value for missing data (matches NF90_FILL_FLOAT)
   real, parameter :: IO_FILL_VALUE = 9.9692099683868690e+36
   
   !> Sentinel value meaning "no fill value set"
   real, parameter :: IO_FILL_UNSET = -huge(1.0)

end module io_constants
