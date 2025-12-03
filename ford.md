---
project: MIAOU - Modular I/O Architecture for Ocean and Unified models
summary: A flexible, modular I/O system for oceanographic models
author: Rachid Benshila
email: rachid.benshila@univ-tlse3.fr
src_dir: .
output_dir: ./doc
page_dir: pages
preprocess: False
exclude_dir: ./miaou_venv
graph: true
warn: true
search: true
display: public
         private
         protected
extensions: F90
---

# MIAOU: Modular I/O Architecture for Ocean and Unified models

## Overview

The MIAOU system provides a flexible, modular input/output architecture designed for oceanographic models. It separates the concerns of:

1. Variable definition and metadata
2. Output configuration and settings
3. I/O operations and file management
4. Backend-specific implementation (e.g., NetCDF)

This separation allows for easy addition of new variables, modification of output settings, and potential integration of alternative output backends.

## Key Features

- Support for variables of different dimensions (0D to 3D)
- Three types of output files: instantaneous (history), time-averaged, and restart
- Configuration via namelist files
- Independent frequency settings for each variable and file type
- Custom file prefixes for specific variables or groups
- Extensible backend architecture (currently implemented with NetCDF)

## Architecture

The system is organized into several modules with clear separation of concerns:

### Core components:
- `io_definitions`: Core types and interfaces
- `io_config`: Configuration management
- `io_manager`: High-level I/O operations

### Backend implementation:
- `netcdf_utils`: Basic NetCDF utilities
- `netcdf_backend`: Generic NetCDF operations
- `io_netcdf`: NetCDF-specific I/O implementation
- `io_netcdf_avg`: NetCDF time averaging

### Variable management:
- `ocean_var`: Basic model variables
- `grid_module`: Grid and dimension definitions
- `var_definitions`: Variable metadata and registration
- `var_registry`: Grid creation and variable registration

## Usage

### Defining new variables

To add a new variable for output, modify the `init_var_definitions` subroutine in `var_definitions.F90`.
Simply add a call to the appropriate `define_Xd_var` function:

```fortran
call define_2d_var("new_var_name", "descriptive name", "units", grid_type, data_pointer)
```

### Configuring output

Edit the namelist file `output_config.nml` to specify:

- Global settings (prefix, default frequencies)
- Per-variable settings (output types, frequencies, custom prefixes)

Example:
```fortran
&output_vars
  var_configs(7) = "new_var", .true., .false., .true., "custom_prefix", 3600., -1., 86400.
/
```

### Running the example

Compile using the provided Makefile and run the test program:

```bash
make
./test_output.exe
```

This will generate NetCDF files according to the configuration.

## API Documentation

Detailed documentation for each module and subroutine is provided through the FORD-generated API documentation.