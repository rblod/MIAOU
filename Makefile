# Makefile for MIAOU - Modular I/O Architecture for Ocean and Unified models
#
# Version 0.4.0 - Refactored with:
# - Centralized constants (io_constants.F90)
# - Centralized error handling (io_error.F90)
# - File naming module (io_naming.F90)
# - Generic averaging module (io_averaging.F90)
# - Single file registry (no duplication)
# - Pointer-based variable access

# Compiler settings
FC = gfortran
FFLAGS = -g -Wall -Wextra -fcheck=all -fbacktrace -O0

# NetCDF configuration
# Adjust NCDF_ROOT to your NetCDF installation
NCDF_ROOT ?= /usr/local
NCDF_INC = -I$(NCDF_ROOT)/include
NCDF_LIB = -L$(NCDF_ROOT)/lib -lnetcdff -lnetcdf

# All flags
FCFLAGS = $(FFLAGS) $(NCDF_INC)
LDFLAGS = $(NCDF_LIB)

# Source files in dependency order
# Level 0: No dependencies
SRCS_L0 = io_constants.F90 io_error.F90 grid_module.F90 ocean_var.F90

# Level 1: Depends on L0
SRCS_L1 = io_naming.F90 netcdf_utils.F90 io_definitions.F90

# Level 2: Depends on L1
SRCS_L2 = netcdf_backend.F90 io_config.F90 io_averaging.F90

# Level 3: Depends on L2
SRCS_L3 = io_netcdf.F90 io_netcdf_avg.F90 var_definitions.F90

# Level 4: Depends on L3
SRCS_L4 = io_manager.F90

# Level 5: Depends on L4
SRCS_L5 = var_registry.F90

# Main program
SRCS_MAIN = main_test_output.F90

# All sources
SRCS = $(SRCS_L0) $(SRCS_L1) $(SRCS_L2) $(SRCS_L3) $(SRCS_L4) $(SRCS_L5) $(SRCS_MAIN)

# Object files
OBJS = $(SRCS:.F90=.o)

# Target executable
TARGET = test_output.exe

# Default target
all: $(TARGET)

# Link
$(TARGET): $(OBJS)
	$(FC) -o $@ $^ $(LDFLAGS)
	@echo "Build successful: $(TARGET)"

# Compile rules with explicit dependencies

# Level 0: No dependencies
io_constants.o: io_constants.F90
	$(FC) $(FCFLAGS) -c $<

io_error.o: io_error.F90
	$(FC) $(FCFLAGS) -c $<

grid_module.o: grid_module.F90
	$(FC) $(FCFLAGS) -c $<

ocean_var.o: ocean_var.F90
	$(FC) $(FCFLAGS) -c $<

# Level 1: Depends on L0
io_naming.o: io_naming.F90 io_constants.o
	$(FC) $(FCFLAGS) -c $<

netcdf_utils.o: netcdf_utils.F90 io_error.o
	$(FC) $(FCFLAGS) -c $<

io_definitions.o: io_definitions.F90 grid_module.o io_constants.o
	$(FC) $(FCFLAGS) -c $<

# Level 2: Depends on L1
netcdf_backend.o: netcdf_backend.F90 netcdf_utils.o grid_module.o
	$(FC) $(FCFLAGS) -c $<

io_config.o: io_config.F90 io_definitions.o io_constants.o
	$(FC) $(FCFLAGS) -c $<

io_averaging.o: io_averaging.F90 io_definitions.o
	$(FC) $(FCFLAGS) -c $<

# Level 3: Depends on L2
io_netcdf.o: io_netcdf.F90 netcdf_utils.o netcdf_backend.o grid_module.o io_definitions.o io_config.o io_constants.o
	$(FC) $(FCFLAGS) -c $<

io_netcdf_avg.o: io_netcdf_avg.F90 io_definitions.o io_averaging.o netcdf_backend.o
	$(FC) $(FCFLAGS) -c $<

var_definitions.o: var_definitions.F90 grid_module.o io_definitions.o ocean_var.o
	$(FC) $(FCFLAGS) -c $<

# Level 4: Depends on L3
io_manager.o: io_manager.F90 io_definitions.o io_config.o io_netcdf.o io_netcdf_avg.o io_constants.o io_naming.o
	$(FC) $(FCFLAGS) -c $<

# Level 5: Depends on L4
var_registry.o: var_registry.F90 grid_module.o io_definitions.o io_manager.o var_definitions.o ocean_var.o
	$(FC) $(FCFLAGS) -c $<

# Main program
main_test_output.o: main_test_output.F90 var_registry.o io_manager.o ocean_var.o grid_module.o
	$(FC) $(FCFLAGS) -c $<

# Clean
clean:
	rm -f *.o *.mod $(TARGET)

# Clean output files too
cleanall: clean
	rm -f *.nc

# Documentation (requires FORD)
doc:
	ford ford.md

# Help
help:
	@echo "MIAOU Build System"
	@echo ""
	@echo "Targets:"
	@echo "  all      - Build the test program (default)"
	@echo "  clean    - Remove object files and executables"
	@echo "  cleanall - Remove object files, executables, and NetCDF outputs"
	@echo "  doc      - Generate documentation (requires FORD)"
	@echo "  help     - Show this message"
	@echo ""
	@echo "Variables:"
	@echo "  NCDF_ROOT - Path to NetCDF installation (default: /usr/local)"
	@echo ""
	@echo "Example:"
	@echo "  make NCDF_ROOT=/opt/netcdf all"

.PHONY: all clean cleanall doc help
