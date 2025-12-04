# MIAOU v5.0.0 Release Notes

**Release Date:** December 4, 2025

## Highlights

MIAOU v5.0.0 is a major release that introduces **zero-copy I/O** for
instantaneous outputs, dramatically reducing memory usage for typical ocean
model configurations.

### 🚀 Key Features

1. **Zero Memory Overhead for Instant Outputs**
   - Data flows directly from model arrays to NetCDF
   - No intermediate buffers for INSTANT files
   - Up to 100% memory savings for instant-only configurations

2. **Developer-Controlled Restart Variables**
   - New `is_restart` flag in `output_vars.inc`
   - Only restart variables consume buffer memory
   - Clear separation between user config and developer choices

3. **Incremental Restart with Rotation**
   - Restart files written progressively like instant files
   - Automatic rotation after N time levels
   - Correct multi-level restart support

## What's New

### Zero-Copy Architecture

In previous versions, all variables were copied to internal buffers before
writing to NetCDF. This doubled memory usage for large 3D fields.

**Before (v4.0.0):**
```
Model Array → Buffer (copy) → NetCDF
              ↑
          Memory cost: 1× per variable
```

**After (v5.0.0):**
```
Model Array → NetCDF (direct write)

          Memory cost: ZERO for instant outputs
```

### Restart Flag in output_vars.inc

The restart behavior is now explicitly declared per variable:

```fortran
! output_vars.inc

! Diagnostic variables - no buffer needed
OUTVAR("wind_speed", "Wind speed", "m/s", grd_scalar, 0, wind_speed, .false.)

! Prognostic variables - buffer for restart
OUTVAR("zeta", "SSH", "m", grd_rho, 2, zeta, .true.)
OUTVAR("temp", "Temperature", "degC", grd_rho3d, 3, temp, .true.)
```

### Incremental Restart Files

Restart files now work like instant files with rotation:

```
freq = 3600, nlevels = 2:

t=3600   → write record 1   [rec1]
t=7200   → write record 2   [rec1, rec2]
t=10800  → rotate, record 1 [rec1]        ← file recreated
t=14400  → write record 2   [rec1, rec2]
...
```

This ensures correct values at each time level (previous version wrote
the same final values to all levels).

## Memory Savings

### Typical CROCO Configuration

Grid: 1000×800×50 points, 10 3D variables (~160 MB each)

| Configuration | v4.0.0 | v5.0.0 | Savings |
|--------------|--------|--------|---------|
| 10 instant variables | 1.6 GB | **0 GB** | 100% |
| 5 instant + 5 averaged | 1.6 GB | 0.8 GB | 50% |
| + 4 restart variables | 1.6 GB | 1.4 GB | 12% |

### When You Benefit Most

- ✅ High-frequency instant outputs (e.g., hourly snapshots)
- ✅ Large 3D fields
- ✅ Many diagnostic variables (not in restart)
- ⚠️ Averaged outputs still require buffers (unavoidable)

## Migration Guide

### Step 1: Update output_vars.inc

Add the 7th argument (restart flag) to all `OUTVAR` declarations:

```fortran
! Old format (v4.0.0)
OUTVAR("zeta", "SSH", "m", grd_rho, 2, zeta)

! New format (v5.0.0)
OUTVAR("zeta", "SSH", "m", grd_rho, 2, zeta, .true.)   ! .true. = restart var
OUTVAR("wind", "Wind", "m/s", grd_scalar, 0, wind, .false.)  ! .false. = no restart
```

### Step 2: Add ensure_files_created()

Call `ensure_files_created()` before the time loop:

```fortran
call initialize_io("output_config.nml")
call init_var_definitions()
call ensure_files_created()   ! ← Add this line

do t = 1, nt
   call send_all_outputs(current_time)
   call write_output(current_time)
end do
```

### Step 3: Update Restart Configuration

For multi-level restart files, use positive frequency:

```fortran
! Namelist
file_name(4) = "restart"
file_freq(4) = 3600.0           ! Write every hour (not -1)
file_restart(4) = .true.
file_restart_nlevels(4) = 2     ! Keep last 2 levels
```

## API Changes

### New Functions

| Function | Description |
|----------|-------------|
| `ensure_files_created()` | Create output files before time loop |
| `nc_write_direct_2d()` | Direct write 2D array to NetCDF |
| `nc_write_direct_3d()` | Direct write 3D array to NetCDF |

### Changed Functions

| Function | Change |
|----------|--------|
| `send_var()` | Now requires `current_time` argument |
| `send_all_outputs()` | Now requires `current_time` argument |

### Removed Functions

| Function | Replacement |
|----------|-------------|
| `is_restart_variable()` | Local `needs_restart_buffer()` in var_definitions |

## Compatibility

### Backward Compatibility

- ❌ **Breaking**: `output_vars.inc` format changed (7 arguments instead of 6)
- ❌ **Breaking**: `ensure_files_created()` must be called before time loop
- ✅ **Compatible**: Namelist format unchanged
- ✅ **Compatible**: NetCDF output format unchanged

### XIOS Compatibility

The architecture is designed for future XIOS backend integration:

```fortran
#ifdef USE_XIOS
   call xios_send_field(name, val)
#else
   call process_var_instant(...)
   call process_var_average(...)
#endif
```

## Known Limitations

1. **Averaged variables still require buffers** - This is inherent to the
   averaging operation and cannot be eliminated.

2. **Restart with freq=-1 produces single record** - For multi-level restart,
   use positive frequency.

3. **No parallel I/O yet** - MPI support planned for v5.1.0.

## Files Modified

| File | Changes |
|------|---------|
| `var_definitions.F90` | Restart flag handling, `needs_restart_buffer()` |
| `io_manager.F90` | `process_var_instant()`, `process_var_average()`, restart rotation |
| `io_netcdf.F90` | `nc_write_direct_*()` functions |
| `io_file_registry.F90` | `data_written_this_step` flag |
| `output_vars.inc` | 7th argument for restart flag |

## Contributors

- Rachid Benshila (LEGOS/CNRS) - Architecture and implementation

## License

MIAOU is part of the CROCO ocean modeling system.
