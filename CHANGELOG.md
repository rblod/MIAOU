# MIAOU - Changelog

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
