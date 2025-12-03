# MIAOU - Changelog

## Version 0.8.0 - 2025-04

### Summary

Six major improvements: variable groups, validation at initialization, 
CF-compliant metadata, restart file support, NetCDF-4 with compression,
and flush/verbosity control.

---

### 1. Variable Groups

**Problem:**  
Repeating the same list of variables in multiple file definitions is error-prone and verbose.

**Solution:**  
Define reusable variable groups that can be referenced with `@groupname`:

```fortran
&io_files_nml
   group_name(1) = "surface"
   group_vars(1) = "zeta,u,v"
   
   file_vars(1) = "@surface,@atm"     ! Expands to: zeta,u,v,wind_speed
/
```

**Files changed:** `io_config.F90`

---

### 2. Configuration Validation

**Problem:**  
Typos in variable names would silently result in missing outputs.

**Solution:**  
`validate_config()` checks all referenced variables exist before file creation.

**Files changed:** `io_manager.F90`

---

### 3. CF-Compliant Metadata

**Problem:**  
Output files lacked standard CF attributes for interoperability.

**Solution:**  
Extended `var_metadata` with `standard_name`, `valid_min/max`, `coordinates`.

**Files changed:** `io_definitions.F90`, `io_netcdf.F90`, `var_definitions.F90`

---

### 4. Restart File Support

**Problem:**  
No dedicated support for restart files.

**Solution:**  
New restart-specific parameters:

```fortran
file_restart(4) = .true.
file_restart_nlevels(4) = 2      ! Write T(n-1) and T(n)
file_double(4) = .true.
file_freq(4) = -1.0              ! -1 = final only
```

**Files changed:** `io_file_registry.F90`, `io_config.F90`, `io_manager.F90`

---

### 5. NetCDF-4 with Compression

**Problem:**  
Large output files, lack of traceability.

**Solution:**  
- NetCDF-4 format with configurable compression
- CF-compliant global attributes
- Creation timestamp in history

```fortran
nml_compression = .true.
nml_compression_level = 4        ! 0-9
```

**Files changed:** `io_netcdf.F90`, `netcdf_backend.F90`, `io_constants.F90`

---

### 6. Flush and Verbosity Control

**Problem:**  
- Long simulations could lose data on crash
- Too much or too little diagnostic output

**Solution:**  

**Periodic flush:** Sync files to disk every N writes to prevent data loss:

```fortran
nml_flush_freq = 10              ! Sync every 10 writes (0=disabled)
```

**Verbosity levels:**

```fortran
nml_verbose = 0                  ! Quiet: errors only
nml_verbose = 1                  ! Normal: configuration + summary (default)
nml_verbose = 2                  ! Debug: every write operation
```

| Level | Output |
|-------|--------|
| 0 (quiet) | Errors and warnings only |
| 1 (normal) | Configuration, file creation, validation |
| 2 (debug) | Every write, flush operations |

**Files changed:** 
- `io_constants.F90` — New variables `io_flush_freq`, `io_verbose`

---

### 7. Unified Error Handling

**Problem:**  
Inconsistent error handling with mix of `print *` statements and ignored return codes.

**Solution:**  
New `io_errors.F90` module providing:

```fortran
call io_error(source, message, fatal=.true.)   ! Fatal error (stops program)
call io_error(source, message, fatal=.false.)  ! Non-fatal error
call io_warning(source, message)               ! Warning (continues)
call io_info(message)                          ! Info (respects verbosity)
call io_debug(source, message)                 ! Debug (only in verbose=2)
```

**Features:**
- Error/warning counters (`io_error_count()`, `io_warning_count()`)
- Consistent formatting with source identification
- Fatal errors show clear banner before stopping
- Respects verbosity level

**Files changed:** 
- `io_errors.F90` — New module
- `io_manager.F90` — Uses new error functions

---

### 8. Proper FillValue Support

**Problem:**  
Fill value was hardcoded to -9999.0 with unreliable detection.

**Solution:**  
- New constants: `IO_FILL_VALUE` (NetCDF standard), `IO_FILL_UNSET` (sentinel)
- `_FillValue` attribute written only when explicitly set by user
- Default: unset (NetCDF uses its own default)

```fortran
call define_2d_var("sst", "SST", "degC", grd, data, fill_value=9.96921e+36)
```

**Files changed:** 
- `io_constants.F90` — New constants
- `io_definitions.F90` — Uses `IO_FILL_UNSET`
- `io_netcdf.F90` — Proper FillValue detection
- `io_config.F90` — Namelist parameters, conditional printing
- `io_manager.F90` — `maybe_flush()` subroutine, verbose messages

---

### 9. Cleanup: Removed Obsolete Constants

Removed from `io_constants.F90`: `IO_TYPE_HIS`, `IO_TYPE_AVG`, `IO_TYPE_RST`

---

### Files Modified

| File | Changes |
|------|---------|
| `io_constants.F90` | Compression, flush, verbose, FillValue constants |
| `io_config.F90` | Groups, restart, flush, verbose namelist |
| `io_errors.F90` | **New** - Unified error handling |
| `io_manager.F90` | Validation, restart, flush, verbose, error handling |
| `io_definitions.F90` | CF-compliant metadata, IO_FILL_UNSET |
| `io_netcdf.F90` | NetCDF-4, CF attributes, FillValue |
| `io_file_registry.F90` | Restart buffer and methods |
| `netcdf_backend.F90` | Compression |
| `var_definitions.F90` | Optional CF arguments |
| `output_config.nml` | Complete example |
| `verify_output.py` | Group/restart support, netCDF4/scipy backend |

---

## Version 0.7.0 - 2025-04

### Summary

Major architectural change: file-centric output configuration replaces variable-centric approach.

---

### 9. File-centric output configuration

**Problem:**  
The previous approach attached output configuration (streams, frequencies) to each variable.
This made it impossible to write the same variable to multiple files with different frequencies
or operations.

**Solution:**  
New file-centric approach where output files are defined separately from variables:

```fortran
! Before (variable-centric):
var%streams(STREAM_HIS)%enabled = .true.
var%streams(STREAM_HIS)%frequency = 3600.0

! After (file-centric):
file_def%name = "hourly"
file_def%frequency = 3600.0
file_def%operation = OP_INSTANT
file_def%variables = ["zeta", "u", "v"]
```

**New module: io_file_registry.F90**

| Type | Description |
|------|-------------|
| `output_file_def` | Defines an output file (name, freq, operation, variable list) |
| `output_file_registry` | Collection of file definitions |
| `avg_state` | Averaging/accumulation state per (file, variable) pair |

**Operations supported:**
- `OP_INSTANT` — Instantaneous values
- `OP_AVERAGE` — Time average
- `OP_MIN` — Minimum over period
- `OP_MAX` — Maximum over period
- `OP_ACCUMULATE` — Sum over period

**New namelist format (io_files_nml):**

```fortran
&io_files_nml
   nml_output_prefix = "ocean"
   nml_time_units = "seconds since 2023-01-01"
   nml_calendar = "gregorian"

   file_name(1) = "hourly"
   file_freq(1) = 3600.0
   file_operation(1) = "instant"
   file_vars(1) = "zeta,u,v"

   file_name(2) = "daily_avg"
   file_freq(2) = 86400.0
   file_operation(2) = "average"
   file_vars(2) = "zeta,temp"
/
```

**Simplified io_variable:**

`io_variable` no longer contains output configuration:

```fortran
type :: io_variable
   type(var_metadata) :: meta    ! Name, units, grid, ndims
   type(var_data_ptr) :: data    ! Pointers to model data
end type
```

**New API:**

| Old | New |
|-----|-----|
| `write_all_data(time)` | `write_output(time)` |
| Per-variable streams | Per-file variable lists |
| `apply_config_to_variable()` | File registry populated from namelist |

**Benefits:**

1. Same variable in multiple files with different frequencies
2. Different operations per file (instant in one, average in another)
3. Configuration closer to XIOS paradigm
4. Cleaner separation: variables = data, files = output config
5. Per-file averaging state (no shared buffers)

**Files changed:**
- `io_definitions.F90` — Simplified (no streams, no avg_buffer)
- `io_config.F90` — New namelist format
- `io_manager.F90` — Rewritten for file-centric logic
- `io_netcdf.F90` — Simplified signatures
- Removed: `io_averaging.F90`, `io_netcdf_avg.F90` (logic now in io_file_registry)

---

## Version 0.6.0 - 2025-04

### Summary

Composition of io_variable into specialized types (var_metadata, var_data_ptr, avg_buffer).

---

## Version 0.5.0 - 2025-04

### Summary

Introduction of the `output_stream` type to unify output configuration.

---

## Version 0.4.0 - 2025-04

### Summary

Decoupling of the averaging module (io_averaging.F90 generic + io_netcdf_avg.F90 backend-specific).

---

## Version 0.3.0 - 2025-04

### Summary

Centralized constants (io_constants.F90) and extracted file naming (io_naming.F90).

---

## Version 0.2.0 - 2025-04

### Summary

Initial refactoring fixing file registry duplication, variable pointer issues, and error handling.
