# MIAOU Changelog

All notable changes to MIAOU are documented in this file.

## [5.4.0] - 2025-12

### Added
- **Configuration validation** (`validate_config()` function):
  - Checks that all variables referenced in output files are registered
  - Warns about empty output files (no variables)
  - Validates frequency values (>0 for non-restart files)
  - Returns error count for programmatic checking
- **New namelist options**:
  - `nml_validate_vars = .true.` — Enable/disable variable existence checking
  - `nml_warn_empty_files = .true.` — Enable/disable empty file warnings
- **New module `io_state.F90`**: Centralizes runtime state
  - `file_registry` — Registry of output files (moved from io_config)
  - `var_registry` — Registry of model variables (moved from io_manager)
  - `reset_io_state()` — Clear all state for unit testing
  - `io_initialized`, `files_created` flags

### Changed
- **State separation** (P2.2):
  - `io_config.F90` now imports `file_registry` from `io_state` instead of declaring it
  - `io_manager.F90` now imports `var_registry` from `io_state` instead of declaring it
  - Both modules re-export their registries for backward compatibility
- Validation is now called explicitly after variable registration (not automatically in create_all_files)

### Improved
- Clear error messages for missing variables: "Variable 'X' in file 'Y' is not registered"
- Validation summary: "Configuration valid: N files, M variables registered"
- Better separation of concerns between parsing and runtime state

## [5.3.0] - 2025-12

### Added
- **Dynamic variable arrays**: Removed hard-coded limits
  - `MAX_VARS` in `var_definitions.F90` → dynamic allocation with automatic growth
  - `MAX_VARS_PER_FILE` in `io_file_registry.F90` → dynamic allocation
  - Arrays grow automatically as needed (starting at 20-50, doubling when full)
- **Unified error handling via `io_error` module**:
  - New error codes for allocation (`IO_ERR_ALLOC`), MPI (`IO_ERR_MPI`), etc.
  - `io_report_error()` and `io_report_warning()` replace scattered `print *` statements
  - Error/warning counts tracked for summary reporting
  - `io_check_nc()` helper for NetCDF status codes
  - `io_error_finalize()` prints summary of errors/warnings
- **`nml_strict_config` namelist option**: When `.true.`, stops on first I/O error instead of logging and continuing
- `cleanup_var_definitions()` subroutine to properly deallocate memory
- `file_init_variables()` method for explicit initialization

### Changed
- **Error handling**: All I/O errors now go through `io_error` module
  - Consistent error messages with location tracking
  - Strict mode stops on first error; normal mode logs and continues
  - Human-readable error code names in messages
- **Memory management**: All arrays use allocatable with dynamic growth
  - No more compile-time limits on variables
  - Proper allocation error checking with error propagation

### Removed
- Hard-coded `MAX_VARS = 100` limit
- Hard-coded `MAX_VARS_PER_FILE = 50` limit
- Unused `MAX_OUTPUT_FILES` constant
- Scattered `print *, "ERROR:"` statements (replaced by io_error)

### Fixed
- Proper error propagation when allocation fails
- Memory leak prevention with proper deallocation in `cleanup_var_definitions()`

## [5.2.0] - 2025-12

### Added
- **Sequential I/O mode** (new default for MPI): Token-passing synchronization allows all processes to write to a single file without requiring parallel NetCDF. Inspired by CROCO's approach.
- **New module `io_mpi_sync.F90`**: Implements token-passing (`io_wait_turn`, `io_pass_turn`) for sequential I/O
- **`nc_open_file` function**: Allows non-master processes to open files created by master
- **Comprehensive test script `run_all_tests.sh`**: Tests all I/O modes (serial, MPI sequential, MPI parallel files, NC4PAR)
- **PARALLEL_FILES CPP key**: Explicitly enables one-file-per-process mode

### Changed
- **Default MPI I/O mode**: Changed from parallel files to sequential I/O (single file)
- **Build targets**:
  - `make mpi` now builds sequential I/O (single file)
  - `make mpi_pf` builds with PARALLEL_FILES (one file per process)
  - `make nc4par` builds with parallel NetCDF-4
- **File naming**: MPI suffix changed from `.NNNN.nc` to `_NNNN.nc` for better compatibility
- **Domain size**: Unified at 40×40×5 for all tests (serial and MPI)

### Known Limitations
- None currently identified for standard usage patterns

### Fixed
- **Restart file rotation in MPI sequential mode**: Added `check_restart_rotation()` to synchronize rotation across all MPI processes before token passing begins. This ensures all processes have consistent `time_index` values, fixing incomplete records after file rotation.
- NC4PAR mode now correctly sets all variables to collective access mode after file creation
- Phase shift removed in NC4PAR mode to ensure verification consistency

## [5.1.0] - 2025-12

### Added
- **MPI parallel I/O support** with CROCO-style 2D domain decomposition
- **New modules**:
  - `mpi_param.F90`: Domain decomposition parameters (NP_XI, NP_ETA, NNODES, LLm, MMm)
  - `mpi_setup.F90`: MPI initialization with neighbor computation
- **Per-process output files**: `ocean_hourly_3600s_0000.nc`, `_0001.nc`, etc.
- **NC4PAR support**: Parallel NetCDF-4 with HDF5 parallel I/O
- **Unified Makefile**: Single Makefile for serial and MPI builds
- Functions: `add_mpi_suffix`, `generate_filename_mpi`, `nc_set_parallel_access`

### Changed
- `io_manager.F90`: Conditional filename suffix based on I/O mode
- `netcdf_backend.F90`: Write with global offsets in NC4PAR mode
- `io_netcdf.F90`: File creation with `NF90_MPIIO` in NC4PAR mode

## [5.0.0] - 2025-04

### Added
- **Zero-copy architecture**: Instant outputs write directly without buffer allocation
- **Restart file support**: Incremental restart with configurable rotation
- **Double buffering** for restart: Maintains N most recent records
- **Restart-specific variables**: `is_restart` flag in variable definitions

### Changed
- Memory optimization: 0 GB for instant-only outputs (vs 1.6 GB in v4.0.0)
- Lazy buffer allocation: Buffers only allocated when needed for averaging

## [4.0.0] - 2025-04

### Added
- **File-centric architecture**: Complete redesign around output files
- **Variable groups**: `@surface`, `@prognostic`, etc. for easy management
- **Multiple operations per file**: instant, average, min, max, accumulate
- **Namelist configuration**: `output_config.nml` with three sections
- **NetCDF-4 compression**: Configurable via namelist

### Changed
- Configuration moved from code to namelist
- Variables can appear in multiple files with different operations
- Averaging state tracked per-file, not per-variable

## [3.0.0] - 2025-04

### Added
- Initial modular I/O system
- Basic NetCDF output support
- Variable registry
- Grid module with axis support

---

## CPP Keys Reference

| Version | Key | Description |
|---------|-----|-------------|
| 5.2.0+ | `MPI` | Enable MPI support |
| 5.2.0+ | `PARALLEL_FILES` | One file per process (with MPI) |
| 5.1.0+ | `NC4PAR` | Parallel NetCDF-4 I/O (requires parallel HDF5) |

## Migration Guide

### From 5.1.0 to 5.2.0

**Build commands changed:**
```bash
# Old (5.1.0)
make mpi      # Built with separate files per process

# New (5.2.0)
make mpi      # Sequential I/O (single file) - NEW DEFAULT
make mpi_pf   # Separate files per process (old behavior)
make nc4par   # Parallel NetCDF-4 (unchanged)
```

**No code changes required** if using the MIAOU API (`write_output`, `send_output`).

### From 5.0.0 to 5.1.0

Add MPI modules to your Makefile:
```makefile
SRCS_MPI = mpi_param.F90 mpi_setup.F90
```

Call MPI initialization before I/O:
```fortran
#ifdef MPI
call mpi_init_decomposition()
#endif
call initialize_io('output_config.nml')
```
