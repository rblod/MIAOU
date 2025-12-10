# MIAOU - Modular I/O Architecture for Ocean and Unified models

**Version 5.2.0**

MIAOU is a modern, modular I/O system designed for ocean and climate models. It provides a flexible, file-centric architecture for managing model outputs with support for multiple output frequencies, averaging operations, and MPI parallelization.

## Key Features

- **File-centric configuration**: Define output files with their own frequencies, variables, and operations
- **Variable groups**: Organize variables into logical groups for easy management
- **Multiple output operations**: Instant, average, min, max, accumulate
- **Restart file support**: Incremental restart with rotation (N most recent records)
- **NetCDF-4 compression**: Configurable compression for reduced file sizes
- **Zero-copy architecture**: Memory-efficient instant outputs
- **MPI Parallel I/O**: Three flexible modes for parallel output

## MPI I/O Modes

MIAOU supports three MPI I/O strategies:

| Mode | Build Command | Output | Requirements |
|------|---------------|--------|--------------|
| **Sequential** (default) | `make mpi` | Single file | Any MPI + NetCDF |
| **Parallel Files** | `make mpi_pf` | One file per process | Any MPI + NetCDF |
| **Parallel NetCDF-4** | `make nc4par` | Single file (parallel writes) | NetCDF with parallel HDF5 |

### Sequential I/O (Default)
Processes write to a single file in turn using token-passing synchronization (like CROCO). This is the default mode, compatible with any NetCDF installation.

### Parallel Files
Each process writes its own file with a `_NNNN` suffix. Post-process with `ncjoin` to combine files.

### Parallel NetCDF-4 (NC4PAR)
All processes write simultaneously to a single file using HDF5 parallel I/O. Requires NetCDF built with `--enable-parallel4`.

## Quick Start

### Serial Build
```bash
make
./test_output.exe
```

### MPI Build (Sequential I/O - Default)
```bash
make mpi
mpirun -np 4 ./test_output.exe
# Output: ocean_hourly_3600s.nc (single file)
```

### MPI Build (Parallel Files)
```bash
make mpi_pf
mpirun -np 4 ./test_output.exe
# Output: ocean_hourly_3600s_0000.nc, ocean_hourly_3600s_0001.nc, ...
ncjoin ocean_hourly_3600s_????.nc  # Combine files
```

### MPI Build (Parallel NetCDF-4)
```bash
# Check support first:
nc-config --has-parallel4  # Should return "yes"

make nc4par
mpirun -np 4 ./test_output.exe
# Output: ocean_hourly_3600s.nc (single file, parallel writes)
```

## Running Tests

A comprehensive test script is provided:

```bash
chmod +x run_all_tests.sh

# Run all tests
./run_all_tests.sh

# Run specific tests
./run_all_tests.sh serial      # Serial only
./run_all_tests.sh mpi         # MPI sequential only
./run_all_tests.sh mpi_pf      # MPI parallel files only
./run_all_tests.sh nc4par      # NC4PAR only

# Run multiple tests
./run_all_tests.sh serial mpi  # Serial and MPI sequential
```

The script will:
1. Build each mode
2. Execute the test program
3. Verify output files with `verify_output.py`
4. Report pass/fail status

## Configuration

Output configuration is defined in `output_config.nml`:

```fortran
&io_nml
   output_prefix = 'ocean'
   time_units = 'seconds since 2000-01-01'
   calendar = 'gregorian'
   compression_enabled = .true.
   compression_level = 4
/

&io_groups_nml
   groups(1) = '@surface: zeta, u, v'
   groups(2) = '@thermo: temp, temp_profile'
   groups(3) = '@prognostic: zeta, u, v, temp'
/

&io_files_nml
   files(1) = 'hourly:   freq=3600,  vars=@surface,wind_speed'
   files(2) = '6hourly:  freq=21600, vars=zeta,temp'
   files(3) = 'daily_avg: freq=86400, op=average, vars=@prognostic,temp_profile'
   files(4) = 'restart:  freq=3600,  vars=@prognostic, restart=true, nlevels=2'
/
```

## Architecture

```
┌─────────────────┐     ┌──────────────────┐
│  Model Code     │────▶│   io_manager     │
│ (send_output)   │     │ (orchestration)  │
└─────────────────┘     └────────┬─────────┘
                                 │
        ┌────────────────────────┼────────────────────────┐
        ▼                        ▼                        ▼
┌───────────────┐      ┌─────────────────┐      ┌─────────────────┐
│  io_config    │      │ io_file_registry │      │   io_netcdf     │
│ (namelist)    │      │  (file defs)     │      │  (NetCDF ops)   │
└───────────────┘      └─────────────────┘      └─────────────────┘
```

## Domain Decomposition (MPI)

Configure in `mpi_param.F90`:

```fortran
integer, parameter :: NP_XI = 2    ! Processes in X direction
integer, parameter :: NP_ETA = 2   ! Processes in Y direction
integer, parameter :: NNODES = 4   ! Total = NP_XI × NP_ETA
integer, parameter :: LLm = 40     ! Global X size
integer, parameter :: MMm = 40     ! Global Y size
integer, parameter :: N = 5        ! Vertical levels
```

## Source Files

| File | Description |
|------|-------------|
| `io_manager.F90` | Main orchestration module |
| `io_config.F90` | Namelist configuration reader |
| `io_netcdf.F90` | NetCDF backend implementation |
| `io_file_registry.F90` | File definitions and state |
| `mpi_param.F90` | MPI parameters and decomposition |
| `mpi_setup.F90` | MPI initialization (CROCO-style) |
| `io_mpi_sync.F90` | Token-passing synchronization |
| `netcdf_backend.F90` | NetCDF write operations |
| `output_config.nml` | Example configuration |
| `run_all_tests.sh` | Comprehensive test script |
| `verify_output.py` | Python verification script |

## Requirements

- Fortran compiler (gfortran, ifort, etc.)
- NetCDF-Fortran library
- MPI (for parallel builds)
- Python 3 + netCDF4 (for verification script)
- NetCDF with parallel HDF5 (for NC4PAR mode only)

### Checking NetCDF Parallel Support

```bash
nc-config --has-parallel4   # Returns "yes" if NC4PAR supported
nc-config --has-hdf5        # Returns "yes" if HDF5 available
```

## CPP Keys

| Key | Description |
|-----|-------------|
| `MPI` | Enable MPI support |
| `PARALLEL_FILES` | One file per process (with MPI) |
| `NC4PAR` | Parallel NetCDF-4 I/O (with MPI) |

## License

MIAOU is released under the CeCILL-C license, compatible with the CROCO ocean model licensing.

## Authors

- MIAOU Team
- LEGOS / CNRS / IRD

## Version History

See [CHANGELOG.md](CHANGELOG.md) for detailed version history.
