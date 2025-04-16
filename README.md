# MIAOU: Modular I/O Architecture for Ocean and Unified models

## Overview

MIAOU is a flexible, modular input/output system designed for oceanographic models. It provides a clean separation of concerns between variable definition, output configuration, and backend implementation. The system supports variables of different dimensions (0D to 3D) and offers multiple output types (instantaneous, time-averaged, and restart files) with configurable frequencies.

## Key Features

- **Flexible Variable Support**: Handle scalar values (0D), profiles (1D), surfaces (2D), and volumes (3D)
- **Multiple Output Types**: Generate history (instantaneous), average, and restart files
- **Configurable Output**: Define output frequency and file prefixes via namelist files
- **Variable-Specific Settings**: Configure each variable independently or use global defaults
- **Modular Design**: Easily extend with new variables or alternative backend implementations
- **NetCDF Backend**: Compliant with CF (Climate and Forecast) metadata conventions

## Directory Structure

```
.
├── ocean_var.F90         # Model variable definitions
├── grid_module.F90       # Grid and axis type definitions
├── io_definitions.F90    # Core I/O types and interfaces
├── io_config.F90         # Configuration handling from namelist
├── var_definitions.F90   # Variable metadata definitions
├── var_registry.F90      # Variable registration and grid creation
├── io_manager.F90        # High-level I/O operations
├── io_netcdf.F90         # NetCDF-specific implementation
├── io_netcdf_avg.F90     # NetCDF time averaging implementation
├── netcdf_backend.F90    # Low-level NetCDF operations
├── netcdf_utils.F90      # NetCDF utility functions
├── main_test_output.F90  # Test program
├── output_config.nml     # Example configuration file
├── Makefile              # Build system
└── ford.md               # FORD documentation configuration
```

## Architecture

MIAOU is built on a layered architecture:

1. **Core Layer**: Basic types, interfaces, and configuration handling
2. **Management Layer**: Variable registration and high-level I/O operations
3. **Backend Layer**: Implementation-specific code (currently NetCDF)

This separation allows changing the backend implementation without affecting the rest of the system.

## Installation

### Prerequisites

- Fortran compiler (tested with gfortran 8.0+)
- NetCDF Fortran library and development files
- FORD (optional, for documentation generation)

### Building

1. Adjust the `NCDF_ROOT` in the Makefile to point to your NetCDF installation
2. Build the test program:

```bash
make
```

3. Generate documentation (optional):

```bash
make doc
```

## Usage

### Running the Example

The test program simulates a simple oceanographic model with variables of different dimensions:

```bash
./test_output.exe
```

This will generate NetCDF output files according to the configuration in `output_config.nml`.

### Adding New Variables

To add a new variable for output:

1. Define the variable in `ocean_var.F90`
2. Add a call to the appropriate `define_Xd_var` function in `init_var_definitions()` in `var_definitions.F90`:

```fortran
call define_2d_var("new_var_name", "descriptive name", "units", grid_type, data_pointer)
```

3. Configure output settings in `output_config.nml`:

```fortran
&output_vars
  var_configs(7) = "new_var_name", .true., .false., .true., "custom_prefix", 3600., -1., 86400.
/
```

### Configuration Options

The `output_config.nml` file contains two namelists:

#### 1. Global Settings

```fortran
&output_global
  output_prefix = "model"     ! Default file prefix
  global_freq_his = 3600.0    ! History file frequency (seconds)
  global_freq_avg = 7200.0    ! Average file frequency (seconds)
  global_freq_rst = 86400.0   ! Restart file frequency (seconds)
  global_to_his = .true.      ! Write to history files by default
  global_to_avg = .false.     ! Write to average files by default
  global_to_rst = .true.      ! Write to restart files by default
/
```

#### 2. Variable-Specific Settings

```fortran
&output_vars
  ! Format: "name", write_his, write_avg, write_rst, "file_prefix", freq_his, freq_avg, freq_rst
  var_configs(1) = "zeta", .true., .true., .false., "", 3600., 7200., -1.
/
```

Each variable setting includes:
- Variable name
- Flags for history, average, and restart output
- Optional custom file prefix
- Output frequencies for each file type (seconds, -1 to disable)

## File Naming Convention

Output files follow the naming pattern:
```
<prefix>_<type>_<frequency>s.nc
```

Example: `ocean_his_3600s.nc` for history output every 3600 seconds with the "ocean" prefix.

## Contributing

Contributions are welcome! When adding features, please maintain the separation of concerns:

- Keep variable definitions separate from I/O operations
- Maintain the abstraction between the I/O manager and backend implementation
- Add appropriate documentation for new features

## License

[Insert your license information here]

## Acknowledgments

[Insert any acknowledgments here]