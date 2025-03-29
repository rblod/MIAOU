# Fortran Modular Output System

This project demonstrates a modular output system for scientific simulations using Fortran. It supports multiple backends (currently NetCDF), and outputs can be configured via namelist files.

## Structure

- `file_manager.F90`: Manages variables and output logic
- `variables_registry.F90`: Declares and registers model variables
- `namelist_output.F90`: Reads output settings from `output_config.nml`
- `output_backend.F90`: Dispatches output to NetCDF or other future backends
- `main_test_output.F90`: Sample program using the infrastructure
- `Makefile`: Compiles the full system
- `output_config.nml`: Configures which variables to write, how, and where

## Requirements

- Fortran compiler (e.g. `gfortran`)
- NetCDF Fortran library

Install on Debian/Ubuntu:

```bash
sudo apt install gfortran libnetcdff-dev
```

## Compilation

```bash
make
```

## Execution

```bash
./test_output.exe
```

## Clean up

```bash
make clean
```
