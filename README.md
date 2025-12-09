# MIAOU: Modular I/O Architecture for Ocean and Unified models

## Overview

MIAOU is a flexible, modular input/output system designed for oceanographic models. It provides a clean separation of concerns between variable definition, output configuration, and backend implementation. 

**Key design principle**: File-centric configuration where output files define which variables they contain, rather than variables defining where they go.

## Key Features

- **File-Centric Configuration**: Define output files with their variables, frequencies, and operations
- **MPI Parallel I/O**: Per-process output files with 2D domain decomposition
- **Variable Groups**: Reusable groups of variables (`@surface`, `@prognostic`)
- **Multiple Operations**: Instantaneous, average, min, max, accumulate
- **Restart Support**: Multi-level time restart files with final-only option
- **NetCDF-4 Compression**: Configurable deflate compression
- **CF-Compliant**: Standard metadata attributes (standard_name, valid_range, coordinates)
- **Validation**: Automatic check of configuration at initialization
- **Flexible Dimensions**: Support for 0D (scalar) to 3D variables
- **Verbosity Control**: Quiet, normal, or debug output levels
- **Periodic Flush**: Protection against data loss during long simulations

## Quick Start

### Building

```bash
# Serial version
make

# MPI parallel version
make mpi
```

### Running

```bash
# Serial
./test_output.exe

# MPI with 4 processes (2×2 decomposition)
mpirun -np 4 ./test_output.exe
```

### MPI Output Files

In MPI mode, each process writes its own file with a `.NNNN.` suffix:

```
ocean_hourly_3600s.0000.nc   # Process 0 (i=1:50, j=1:40)
ocean_hourly_3600s.0001.nc   # Process 1 (i=51:100, j=1:40)
ocean_hourly_3600s.0002.nc   # Process 2 (i=1:50, j=41:80)
ocean_hourly_3600s.0003.nc   # Process 3 (i=51:100, j=41:80)
```

Use `ncjoin` from CROCO tools to reassemble:
```bash
ncjoin ocean_hourly_3600s.????.nc -o ocean_hourly_3600s.nc
```

### Basic Configuration

```fortran
&io_files_nml
   nml_output_prefix = "ocean"
   
   ! Define variable groups
   group_name(1) = "surface"
   group_vars(1) = "zeta,u,v"
   
   ! Define output files
   file_name(1) = "hourly"
   file_freq(1) = 3600.0
   file_operation(1) = "instant"
   file_vars(1) = "@surface"
/
```

## Architecture

```
┌─────────────────┐     ┌─────────────────┐     ┌─────────────────┐
│   io_manager    │────▶│   io_config     │────▶│   io_netcdf     │
│  (orchestration)│     │ (configuration) │     │    (backend)    │
└─────────────────┘     └─────────────────┘     └─────────────────┘
         │                      │                        │
         ▼                      ▼                        ▼
┌─────────────────┐     ┌─────────────────┐     ┌─────────────────┐
│  io_definitions │     │io_file_registry │     │ netcdf_backend  │
│   (var types)   │     │  (file types)   │     │  (low-level)    │
└─────────────────┘     └─────────────────┘     └─────────────────┘
```

## Directory Structure

```
.
├── io_constants.F90      # Global constants (lengths, compression, verbose)
├── io_error.F90          # Error handling
├── grid_module.F90       # Grid and axis definitions
├── io_definitions.F90    # Variable type definitions
├── io_file_registry.F90  # File and averaging state types
├── io_config.F90         # Namelist parsing, group expansion
├── io_naming.F90         # Filename generation
├── io_manager.F90        # High-level I/O coordination
├── io_netcdf.F90         # NetCDF implementation
├── netcdf_backend.F90    # Low-level NetCDF operations
├── netcdf_utils.F90      # NetCDF utilities
├── var_definitions.F90   # Variable definition helpers
├── var_registry.F90      # Grid creation, variable registration
├── ocean_var.F90         # Model variables (example)
├── main_test_output.F90  # Test program
├── output_config.nml     # Configuration file
└── Makefile
```

## Configuration Reference

### Global Settings

```fortran
&io_files_nml
   nml_output_prefix = "ocean"                    ! File prefix
   nml_time_units = "seconds since 2023-01-01"    ! CF time units
   nml_calendar = "gregorian"                     ! Calendar type
   
   ! Compression (NetCDF-4)
   nml_compression = .true.                       ! Enable compression
   nml_compression_level = 4                      ! 0-9 (0=none, 9=max)
   
   ! Diagnostics
   nml_flush_freq = 0                             ! Flush every N writes (0=disabled)
   nml_verbose = 1                                ! 0=quiet, 1=normal, 2=debug
/
```

### Variable Groups

```fortran
   group_name(1) = "surface"
   group_vars(1) = "zeta,u,v"
   
   group_name(2) = "prognostic"
   group_vars(2) = "zeta,u,v,temp"
```

### Output Files

```fortran
   ! Hourly instantaneous output
   file_name(1) = "hourly"
   file_freq(1) = 3600.0              ! Frequency in seconds
   file_operation(1) = "instant"      ! instant, average, min, max, accumulate
   file_vars(1) = "@surface,wind"     ! Groups and/or individual variables
   
   ! Daily averages
   file_name(2) = "daily_avg"
   file_freq(2) = 86400.0
   file_operation(2) = "average"
   file_vars(2) = "@surface,temp"
   
   ! Restart file (final only)
   file_name(3) = "restart"
   file_freq(3) = -1.0                ! -1 = final only
   file_operation(3) = "instant"
   file_vars(3) = "@prognostic"
   file_restart(3) = .true.
   file_restart_nlevels(3) = 2        ! Store 2 time levels
   file_double(3) = .true.            ! Double precision
```

### File Operations

| Operation | Description |
|-----------|-------------|
| `instant` | Write current values |
| `average` | Time-averaged values |
| `min` | Minimum over period |
| `max` | Maximum over period |
| `accumulate` | Sum over period |

## Adding New Variables

### 1. Define the variable data (ocean_var.F90)

```fortran
real, dimension(:,:), allocatable, target :: my_new_var
```

### 2. Register the variable (var_definitions.F90)

```fortran
call define_2d_var("my_var", "My Variable Description", "units", &
                   grd_rho, my_new_var, &
                   standard_name="standard_cf_name", &    ! Optional
                   valid_min=0.0, valid_max=100.0)        ! Optional
```

### 3. Add to configuration (output_config.nml)

```fortran
   ! Add to a group
   group_vars(1) = "zeta,u,v,my_var"
   
   ! Or reference directly in a file
   file_vars(1) = "@surface,my_var"
```

## Output Files

### Naming Convention

```
<prefix>_<name>_<frequency>s.nc
```

Examples:
- `ocean_hourly_3600s.nc`
- `ocean_daily_avg_86400s.nc`
- `ocean_restart.nc`

### Global Attributes

All files include CF-compliant global attributes:

```
:Conventions = "CF-1.8"
:title = "MIAOU ocean model output"
:institution = "LEGOS/CNRS"
:source = "MIAOU v0.8.0"
:history = "2025-04-15T14:30:22 Created by MIAOU I/O system"
:file_name = "hourly"
:output_frequency_seconds = 3600.0
```

## API Usage

### Basic Workflow

```fortran
program my_model
   use io_manager
   use var_registry
   
   ! 1. Initialize I/O system
   call initialize_io("output_config.nml")
   
   ! 2. Create grids and register variables
   call init_variables()
   
   ! 3. Time loop
   do while (time <= end_time)
      ! ... model computations ...
      
      ! Write output (handled automatically based on frequencies)
      call write_output(time, is_final=(time >= end_time))
      
      time = time + dt
   end do
   
   ! 4. Cleanup
   call finalize_io()
end program
```

## Extending MIAOU

### Adding a New Backend

To add support for HDF5, ADIOS2, or another format:

1. Create `hdf5_backend.F90` with same interface as `netcdf_backend.F90`
2. Create `io_hdf5.F90` with same interface as `io_netcdf.F90`
3. Modify `io_manager.F90` to select backend based on configuration

The current architecture is designed for this extension (planned for v2.0).

## Version History

- **v0.8.0** (2025-04): File-centric architecture, groups, validation, CF metadata, restart support, compression, flush, verbosity
- **v0.7.0** (2025-04): File-centric refactoring
- **v0.6.0** (2025-04): Variable composition types

See [CHANGELOG.md](CHANGELOG.md) for detailed changes.
>>>>>>> 352d4fb (Ajout fichiers restants de A)

## Key Features

<<<<<<< HEAD
- **File-Centric Configuration**: Define output files with their variables, frequencies, and operations
- **Variable Groups**: Reusable groups of variables (`@surface`, `@prognostic`)
- **Multiple Operations**: Instantaneous, average, min, max, accumulate
- **Restart Support**: Multi-level time restart files with final-only option
- **NetCDF-4 Compression**: Configurable deflate compression
- **CF-Compliant**: Standard metadata attributes (standard_name, valid_range, coordinates)
- **Validation**: Automatic check of configuration at initialization
- **Flexible Dimensions**: Support for 0D (scalar) to 3D variables
- **Verbosity Control**: Quiet, normal, or debug output levels
- **Periodic Flush**: Protection against data loss during long simulations

## Quick Start

### Building

```bash
# Adjust NCDF_ROOT in Makefile to your NetCDF installation
make
```

### Running

```bash
./test_output.exe
```

### Basic Configuration

```fortran
&io_files_nml
   nml_output_prefix = "ocean"
   
   ! Define variable groups
   group_name(1) = "surface"
   group_vars(1) = "zeta,u,v"
   
   ! Define output files
   file_name(1) = "hourly"
   file_freq(1) = 3600.0
   file_operation(1) = "instant"
   file_vars(1) = "@surface"
/
```

## Architecture

```
┌─────────────────┐     ┌─────────────────┐     ┌─────────────────┐
│   io_manager    │────▶│   io_config     │────▶│   io_netcdf     │
│  (orchestration)│     │ (configuration) │     │    (backend)    │
└─────────────────┘     └─────────────────┘     └─────────────────┘
         │                      │                        │
         ▼                      ▼                        ▼
┌─────────────────┐     ┌─────────────────┐     ┌─────────────────┐
│  io_definitions │     │io_file_registry │     │ netcdf_backend  │
│   (var types)   │     │  (file types)   │     │  (low-level)    │
└─────────────────┘     └─────────────────┘     └─────────────────┘
```

## Directory Structure

```
.
├── io_constants.F90      # Global constants (lengths, compression, verbose)
├── io_error.F90          # Error handling
├── grid_module.F90       # Grid and axis definitions
├── io_definitions.F90    # Variable type definitions
├── io_file_registry.F90  # File and averaging state types
├── io_config.F90         # Namelist parsing, group expansion
├── io_naming.F90         # Filename generation
├── io_manager.F90        # High-level I/O coordination
├── io_netcdf.F90         # NetCDF implementation
├── netcdf_backend.F90    # Low-level NetCDF operations
├── netcdf_utils.F90      # NetCDF utilities
├── var_definitions.F90   # Variable definition helpers
├── var_registry.F90      # Grid creation, variable registration
├── ocean_var.F90         # Model variables (example)
├── main_test_output.F90  # Test program
├── output_config.nml     # Configuration file
└── Makefile
```

## Configuration Reference

### Global Settings

```fortran
&io_files_nml
   nml_output_prefix = "ocean"                    ! File prefix
   nml_time_units = "seconds since 2023-01-01"    ! CF time units
   nml_calendar = "gregorian"                     ! Calendar type
   
   ! Compression (NetCDF-4)
   nml_compression = .true.                       ! Enable compression
   nml_compression_level = 4                      ! 0-9 (0=none, 9=max)
   
   ! Diagnostics
   nml_flush_freq = 0                             ! Flush every N writes (0=disabled)
   nml_verbose = 1                                ! 0=quiet, 1=normal, 2=debug
/
```

### Variable Groups

```fortran
   group_name(1) = "surface"
   group_vars(1) = "zeta,u,v"
   
   group_name(2) = "prognostic"
   group_vars(2) = "zeta,u,v,temp"
```

### Output Files

```fortran
   ! Hourly instantaneous output
   file_name(1) = "hourly"
   file_freq(1) = 3600.0              ! Frequency in seconds
   file_operation(1) = "instant"      ! instant, average, min, max, accumulate
   file_vars(1) = "@surface,wind"     ! Groups and/or individual variables
   
   ! Daily averages
   file_name(2) = "daily_avg"
   file_freq(2) = 86400.0
   file_operation(2) = "average"
   file_vars(2) = "@surface,temp"
   
   ! Restart file (final only)
   file_name(3) = "restart"
   file_freq(3) = -1.0                ! -1 = final only
   file_operation(3) = "instant"
   file_vars(3) = "@prognostic"
   file_restart(3) = .true.
   file_restart_nlevels(3) = 2        ! Store 2 time levels
   file_double(3) = .true.            ! Double precision
```

### File Operations

| Operation | Description |
|-----------|-------------|
| `instant` | Write current values |
| `average` | Time-averaged values |
| `min` | Minimum over period |
| `max` | Maximum over period |
| `accumulate` | Sum over period |

## Adding New Variables

### 1. Define the variable data (ocean_var.F90)

```fortran
real, dimension(:,:), allocatable, target :: my_new_var
```

### 2. Register the variable (var_definitions.F90)

```fortran
call define_2d_var("my_var", "My Variable Description", "units", &
                   grd_rho, my_new_var, &
                   standard_name="standard_cf_name", &    ! Optional
                   valid_min=0.0, valid_max=100.0)        ! Optional
```

### 3. Add to configuration (output_config.nml)

```fortran
   ! Add to a group
   group_vars(1) = "zeta,u,v,my_var"
   
   ! Or reference directly in a file
   file_vars(1) = "@surface,my_var"
```

## Output Files

### Naming Convention

```
<prefix>_<name>_<frequency>s.nc
```

Examples:
- `ocean_hourly_3600s.nc`
- `ocean_daily_avg_86400s.nc`
- `ocean_restart.nc`

### Global Attributes

All files include CF-compliant global attributes:

```
:Conventions = "CF-1.8"
:title = "MIAOU ocean model output"
:institution = "LEGOS/CNRS"
:source = "MIAOU v0.8.0"
:history = "2025-04-15T14:30:22 Created by MIAOU I/O system"
:file_name = "hourly"
:output_frequency_seconds = 3600.0
```

## API Usage

### Basic Workflow

```fortran
program my_model
   use io_manager
   use var_registry
   
   ! 1. Initialize I/O system
   call initialize_io("output_config.nml")
   
   ! 2. Create grids and register variables
   call init_variables()
   
   ! 3. Time loop
   do while (time <= end_time)
      ! ... model computations ...
      
      ! Write output (handled automatically based on frequencies)
      call write_output(time, is_final=(time >= end_time))
      
      time = time + dt
   end do
   
   ! 4. Cleanup
   call finalize_io()
end program
```

## Extending MIAOU

### Adding a New Backend

To add support for HDF5, ADIOS2, or another format:

1. Create `hdf5_backend.F90` with same interface as `netcdf_backend.F90`
2. Create `io_hdf5.F90` with same interface as `io_netcdf.F90`
3. Modify `io_manager.F90` to select backend based on configuration

The current architecture is designed for this extension (planned for v2.0).

## Version History

- **v0.8.0** (2025-04): File-centric architecture, groups, validation, CF metadata, restart support, compression, flush, verbosity
- **v0.7.0** (2025-04): File-centric refactoring
- **v0.6.0** (2025-04): Variable composition types

See [CHANGELOG.md](CHANGELOG.md) for detailed changes.

## Requirements

- Fortran compiler (gfortran 8.0+ or ifort 19.0+)
- NetCDF-Fortran library (4.5+)
- FORD (optional, for documentation)

## License

[To be defined]

## Authors

- Rachid Benshila (LEGOS/CNRS)

## Acknowledgments

Developed for the CROCO ocean modeling community.
