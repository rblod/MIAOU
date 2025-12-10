# MIAOU Makefile
# Modular I/O Architecture for Ocean and Unified models
#
# Usage:
#   make           - Build serial version
#   make mpi       - Build MPI version (sequential I/O, single file - DEFAULT)
#   make mpi_pf    - Build MPI with PARALLEL_FILES (one file per process)
#   make nc4par    - Build MPI with parallel NetCDF-4 (single file, parallel writes)
#   make test      - Run serial test
#   make test_mpi  - Run MPI test with 4 processes
#   make clean     - Clean build files
#   make cleanall  - Clean build files and NetCDF outputs
#
# MPI I/O Modes:
#   - mpi:     Sequential token-passing (like CROCO default). Single file.
#              Each process writes in turn. Works with any NetCDF.
#   - mpi_pf:  Parallel files. Each process writes its own file_NNNN.nc.
#              Post-process with ncjoin. Works with any NetCDF.
#   - nc4par:  Parallel NetCDF-4. Single file with parallel HDF5 writes.
#              Requires NetCDF built with --enable-parallel4.

#===============================================================================
# Compiler configuration
#===============================================================================

# Serial compiler
FC = gfortran
FFLAGS_BASE = -g -Wall -Wextra -fcheck=all -fbacktrace -O0 -cpp

# MPI compiler (uses gfortran underneath)
MPIF90 = mpif90

# NetCDF configuration
NCDF_INC = $(shell /usr/local/bin/nf-config --fflags 2>/dev/null || echo "-I/usr/local/include")
NCDF_LIB = $(shell /usr/local/bin/nf-config --flibs 2>/dev/null || echo "-L/usr/local/lib -lnetcdff -lnetcdf")

#===============================================================================
# Source files by dependency level
#===============================================================================

# MPI modules (only compiled in MPI mode)
SRCS_MPI = mpi_param.F90 mpi_setup.F90 io_mpi_sync.F90

# Core modules (always compiled)
SRCS_L0 = io_constants.F90 io_error.F90 grid_module.F90
SRCS_L1 = io_naming.F90 netcdf_utils.F90 io_definitions.F90
SRCS_L2 = netcdf_backend.F90 io_file_registry.F90
SRCS_L3 = io_netcdf.F90 io_config.F90
SRCS_L4 = io_manager.F90
SRCS_L5 = ocean_var.F90
SRCS_L6 = var_definitions.F90
SRCS_L7 = var_registry.F90
SRCS_L8 = main_test_output.F90

# All sources (serial)
SRCS_SERIAL = $(SRCS_L0) $(SRCS_L1) $(SRCS_L2) $(SRCS_L3) $(SRCS_L4) $(SRCS_L5) $(SRCS_L6) $(SRCS_L7) $(SRCS_L8)
OBJS_SERIAL = $(SRCS_SERIAL:.F90=.o)

# All sources (MPI) - MPI modules first
SRCS_PARALLEL = $(SRCS_MPI) $(SRCS_L0) $(SRCS_L1) $(SRCS_L2) $(SRCS_L3) $(SRCS_L4) $(SRCS_L5) $(SRCS_L6) $(SRCS_L7) $(SRCS_L8)
OBJS_PARALLEL = $(SRCS_PARALLEL:.F90=.o)

# Target executable
TARGET = test_output.exe

#===============================================================================
# Build flags
#===============================================================================

# Serial flags
FFLAGS_SERIAL = $(FFLAGS_BASE) $(NCDF_INC)
LDFLAGS = $(NCDF_LIB)

# MPI flags - sequential I/O (default, single file, token passing)
FFLAGS_MPI = $(FFLAGS_BASE) $(NCDF_INC) -DMPI

# MPI flags - parallel files (one file per process)
FFLAGS_MPI_PF = $(FFLAGS_BASE) $(NCDF_INC) -DMPI -DPARALLEL_FILES

# NC4PAR flags (parallel NetCDF-4, single shared file)
FFLAGS_NC4PAR = $(FFLAGS_BASE) $(NCDF_INC) -DMPI -DNC4PAR

#===============================================================================
# Default target: serial build
#===============================================================================

all: serial

serial: FFLAGS = $(FFLAGS_SERIAL)
serial: $(TARGET)_serial

$(TARGET)_serial: $(OBJS_SERIAL)
	$(FC) $(FFLAGS_SERIAL) -o $(TARGET) $(OBJS_SERIAL) $(LDFLAGS)
	@echo ""
	@echo "Built serial version: $(TARGET)"

#===============================================================================
# MPI target - Sequential I/O (DEFAULT)
#===============================================================================

mpi: clean_objs
	@$(MAKE) build_mpi FFLAGS="$(FFLAGS_MPI)"

build_mpi: $(TARGET)_mpi

$(TARGET)_mpi: $(OBJS_PARALLEL)
	$(MPIF90) $(FFLAGS) -o $(TARGET) $(OBJS_PARALLEL) $(LDFLAGS)
	@echo ""
	@echo "Built MPI version (sequential I/O): $(TARGET)"
	@echo "Run with: mpirun -np 4 ./$(TARGET)"
	@echo "Output: ocean_*.nc (single file, processes write in turn)"

#===============================================================================
# MPI target - Parallel Files (one per process)
#===============================================================================

mpi_pf: clean_objs
	@$(MAKE) build_mpi_pf FFLAGS="$(FFLAGS_MPI_PF)"

build_mpi_pf: $(TARGET)_mpi_pf

$(TARGET)_mpi_pf: $(OBJS_PARALLEL)
	$(MPIF90) $(FFLAGS) -o $(TARGET) $(OBJS_PARALLEL) $(LDFLAGS)
	@echo ""
	@echo "Built MPI version (parallel files): $(TARGET)"
	@echo "Run with: mpirun -np 4 ./$(TARGET)"
	@echo "Output: ocean_*_NNNN.nc (one file per process)"
	@echo "Post-process: ncjoin ocean_hourly_3600s_????.nc"

#===============================================================================
# NC4PAR target (parallel NetCDF-4, single file)
#===============================================================================

nc4par: clean_objs
	@$(MAKE) build_nc4par FFLAGS="$(FFLAGS_NC4PAR)"

build_nc4par: $(TARGET)_nc4par

$(TARGET)_nc4par: $(OBJS_PARALLEL)
	$(MPIF90) $(FFLAGS) -o $(TARGET) $(OBJS_PARALLEL) $(LDFLAGS)
	@echo ""
	@echo "Built NC4PAR version (parallel NetCDF-4): $(TARGET)"
	@echo "Run with: mpirun -np 4 ./$(TARGET)"
	@echo "Output: ocean_*.nc (single file, parallel writes)"
	@echo ""
	@echo "NOTE: Requires NetCDF built with parallel HDF5 support"

#===============================================================================
# Compilation rules
#===============================================================================

# MPI modules (only in MPI mode)
mpi_param.o: mpi_param.F90
	$(MPIF90) $(FFLAGS) -c $<

mpi_setup.o: mpi_setup.F90 mpi_param.o
	$(MPIF90) $(FFLAGS) -c $<

io_mpi_sync.o: io_mpi_sync.F90 mpi_param.o
	$(MPIF90) $(FFLAGS) -c $<

# Level 0 - No dependencies
io_constants.o: io_constants.F90
	$(if $(findstring -DMPI,$(FFLAGS)),$(MPIF90),$(FC)) $(FFLAGS) -c $<

io_error.o: io_error.F90
	$(if $(findstring -DMPI,$(FFLAGS)),$(MPIF90),$(FC)) $(FFLAGS) -c $<

grid_module.o: grid_module.F90
	$(if $(findstring -DMPI,$(FFLAGS)),$(MPIF90),$(FC)) $(FFLAGS) -c $<

# Level 1
io_naming.o: io_naming.F90 io_constants.o
	$(if $(findstring -DMPI,$(FFLAGS)),$(MPIF90),$(FC)) $(FFLAGS) -c $<

netcdf_utils.o: netcdf_utils.F90
	$(if $(findstring -DMPI,$(FFLAGS)),$(MPIF90),$(FC)) $(FFLAGS) -c $<

io_definitions.o: io_definitions.F90 io_constants.o grid_module.o
	$(if $(findstring -DMPI,$(FFLAGS)),$(MPIF90),$(FC)) $(FFLAGS) -c $<

# Level 2
netcdf_backend.o: netcdf_backend.F90 netcdf_utils.o grid_module.o
	$(if $(findstring -DMPI,$(FFLAGS)),$(MPIF90),$(FC)) $(FFLAGS) -c $<

io_file_registry.o: io_file_registry.F90 io_constants.o
	$(if $(findstring -DMPI,$(FFLAGS)),$(MPIF90),$(FC)) $(FFLAGS) -c $<

# Level 3
io_netcdf.o: io_netcdf.F90 io_constants.o netcdf_utils.o netcdf_backend.o grid_module.o io_definitions.o io_file_registry.o
	$(if $(findstring -DMPI,$(FFLAGS)),$(MPIF90),$(FC)) $(FFLAGS) -c $<

io_config.o: io_config.F90 io_constants.o io_file_registry.o
	$(if $(findstring -DMPI,$(FFLAGS)),$(MPIF90),$(FC)) $(FFLAGS) -c $<

# Level 4 - io_manager depends on mpi_io in sequential mode
io_manager.o: io_manager.F90 io_constants.o io_definitions.o io_config.o io_file_registry.o io_naming.o io_netcdf.o
	$(if $(findstring -DMPI,$(FFLAGS)),$(MPIF90),$(FC)) $(FFLAGS) -c $<

# Level 5 - ocean_var depends on mpi_param in MPI mode
ocean_var.o: ocean_var.F90
	$(if $(findstring -DMPI,$(FFLAGS)),$(MPIF90),$(FC)) $(FFLAGS) -c $<

# Level 6
var_definitions.o: var_definitions.F90 grid_module.o io_definitions.o ocean_var.o io_manager.o output_vars.inc
	$(if $(findstring -DMPI,$(FFLAGS)),$(MPIF90),$(FC)) $(FFLAGS) -c $<

# Level 7
var_registry.o: var_registry.F90 grid_module.o io_manager.o var_definitions.o ocean_var.o
	$(if $(findstring -DMPI,$(FFLAGS)),$(MPIF90),$(FC)) $(FFLAGS) -c $<

# Level 8 - Main program
main_test_output.o: main_test_output.F90 var_registry.o io_manager.o ocean_var.o grid_module.o var_definitions.o
	$(if $(findstring -DMPI,$(FFLAGS)),$(MPIF90),$(FC)) $(FFLAGS) -c $<

#===============================================================================
# Test targets
#===============================================================================

test: serial
	./$(TARGET)
	@echo ""
	@ls -la *.nc 2>/dev/null || echo "No NetCDF files found"

test_mpi: mpi
	mpirun -np 4 ./$(TARGET)
	@echo ""
	@ls -la *.nc 2>/dev/null || echo "No NetCDF files found"

test_mpi_pf: mpi_pf
	mpirun -np 4 ./$(TARGET)
	@echo ""
	@ls -la *.nc 2>/dev/null || echo "No NetCDF files found"

test_nc4par: nc4par
	mpirun -np 4 ./$(TARGET)
	@echo ""
	@ls -la *.nc 2>/dev/null || echo "No NetCDF files found"

#===============================================================================
# Clean targets
#===============================================================================

clean_objs:
	rm -f *.o *.mod

clean: clean_objs
	rm -f $(TARGET)

cleanall: clean
	rm -f *.nc

.PHONY: all serial mpi build_mpi mpi_pf build_mpi_pf nc4par build_nc4par \
        test test_mpi test_mpi_pf test_nc4par clean clean_objs cleanall
