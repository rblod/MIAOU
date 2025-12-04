# MIAOU Changelog

All notable changes to the MIAOU I/O system are documented in this file.

## [5.0.0] - 2025-12-04

### ⚡ Major Release: Zero-Copy Architecture

This release introduces a fundamental redesign of the I/O system to eliminate
unnecessary memory duplication for instantaneous outputs.

### Added

- **Zero-copy writes for INSTANT files**: Data is written directly from model
  arrays to NetCDF without intermediate buffers
- **Restart flag in output_vars.inc**: 7th argument `is_restart` controls
  whether a variable needs buffering for restart files
- **Incremental restart with rotation**: Restart files are written
  incrementally and rotate after N time levels
- **`ensure_files_created()` API**: Explicit file creation before time loop
- **`needs_restart_buffer()` function**: Fast local lookup for restart status
- **Direct write functions**: `nc_write_direct_0d/1d/2d/3d()` in io_netcdf.F90

### Changed

- **`send_var()` signature**: Now includes `current_time` parameter
- **Buffer allocation strategy**: Buffers only allocated for:
  - Variables in AVERAGE files (for accumulation)
  - Variables marked `is_restart=.true.` (for restart)
- **Restart file behavior**: Now writes incrementally like INSTANT files,
  with automatic rotation when reaching N levels
- **`output_vars.inc` format**: Added 7th argument for restart flag

### Removed

- Automatic buffer allocation for all variables
- `is_restart_variable()` public function (replaced by local lookup)
- Old `write_restart_file()` bulk write approach

### Performance

Memory savings for typical CROCO configuration (1000×800×50, 10 3D variables):

| Configuration | v4.0.0 | v5.0.0 | Savings |
|--------------|--------|--------|---------|
| All instant | 1.6 GB | 0 GB | 100% |
| Mixed instant/avg | 1.6 GB | 0.8 GB | 50% |

### Migration Guide

1. Update `output_vars.inc` to include restart flag:
   ```fortran
   ! Before (v4.0.0)
   OUTVAR("zeta", "SSH", "m", grd_rho, 2, zeta)
   
   ! After (v5.0.0)
   OUTVAR("zeta", "SSH", "m", grd_rho, 2, zeta, .true.)
   ```

2. Add `ensure_files_created()` before time loop:
   ```fortran
   call ensure_files_created()
   do t = 1, nt
      ...
   end do
   ```

3. For restart with multiple time levels, use `freq > 0`:
   ```fortran
   file_freq(4) = 3600.0    ! Write every hour
   file_restart_nlevels(4) = 2  ! Keep last 2 records
   ```

---

## [4.0.0] - 2025-12-03

### Added

- **Single-file variable definition**: All variables defined in `output_vars.inc`
- **Preprocessor macro approach**: `OUTVAR()` macro for clean syntax
- **Internal buffers**: Variables don't need `target` attribute
- **Variable groups**: `@groupname` syntax in namelist

### Changed

- Unified API through `send_all_outputs()` and `write_output()`
- Removed requirement for `target` attribute on model variables

### Known Issues

- All variables buffered regardless of output type (fixed in v5.0.0)

---

## [3.0.0] - 2025-12-02

### Added

- File-centric configuration via namelist
- Support for INSTANT, AVERAGE, and RESTART operations
- Compression support (NetCDF-4)
- CF-1.8 compliant output

---

## [2.0.0] - 2025-12-01

### Added

- Grid-based variable registration
- Automatic dimension handling
- Time coordinate management

---

## [1.0.0] - 2025-11-30

### Added

- Initial release
- Basic NetCDF output support
- Manual variable registration
