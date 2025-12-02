# MIAOU Makefile
# Modular I/O Architecture for Ocean and Unified models

FC = gfortran
FFLAGS = -g -Wall -Wextra -fcheck=all -fbacktrace -O0

# NetCDF configuration
NCDF_ROOT ?= /usr/local
NCDF_INC = -I$(NCDF_ROOT)/include
NCDF_LIB = -L$(NCDF_ROOT)/lib -lnetcdff -lnetcdf

FFLAGS += $(NCDF_INC)
LDFLAGS = $(NCDF_LIB)

# Source files by dependency level
# Level 0: No dependencies on other MIAOU modules
SRCS_L0 = io_constants.F90 io_error.F90 grid_module.F90 ocean_var.F90

# Level 1: Depends on L0
SRCS_L1 = io_naming.F90 netcdf_utils.F90 io_definitions.F90

# Level 2: Depends on L0-L1
SRCS_L2 = netcdf_backend.F90 io_file_registry.F90

# Level 3: Depends on L0-L2
SRCS_L3 = io_netcdf.F90 io_config.F90

# Level 4: Depends on L0-L3
SRCS_L4 = var_definitions.F90

# Level 5: Depends on L0-L4
SRCS_L5 = io_manager.F90

# Level 6: Depends on L0-L5
SRCS_L6 = var_registry.F90

# Level 7: Main program
SRCS_L7 = main_test_output.F90

# All sources
SRCS = $(SRCS_L0) $(SRCS_L1) $(SRCS_L2) $(SRCS_L3) $(SRCS_L4) $(SRCS_L5) $(SRCS_L6) $(SRCS_L7)
OBJS = $(SRCS:.F90=.o)

# Target executable
TARGET = test_output.exe

# Default target
all: $(TARGET)

$(TARGET): $(OBJS)
	$(FC) $(FFLAGS) -o $@ $(OBJS) $(LDFLAGS)

# Compilation rules by level
# Level 0
io_constants.o: io_constants.F90
	$(FC) $(FFLAGS) -c $<

io_error.o: io_error.F90
	$(FC) $(FFLAGS) -c $<

grid_module.o: grid_module.F90
	$(FC) $(FFLAGS) -c $<

ocean_var.o: ocean_var.F90
	$(FC) $(FFLAGS) -c $<

# Level 1
io_naming.o: io_naming.F90 io_constants.o
	$(FC) $(FFLAGS) -c $<

netcdf_utils.o: netcdf_utils.F90
	$(FC) $(FFLAGS) -c $<

io_definitions.o: io_definitions.F90 io_constants.o grid_module.o
	$(FC) $(FFLAGS) -c $<

# Level 2
netcdf_backend.o: netcdf_backend.F90 netcdf_utils.o grid_module.o
	$(FC) $(FFLAGS) -c $<

io_file_registry.o: io_file_registry.F90 io_constants.o
	$(FC) $(FFLAGS) -c $<

# Level 3
io_netcdf.o: io_netcdf.F90 io_constants.o netcdf_utils.o netcdf_backend.o grid_module.o io_definitions.o io_file_registry.o
	$(FC) $(FFLAGS) -c $<

io_config.o: io_config.F90 io_constants.o io_file_registry.o
	$(FC) $(FFLAGS) -c $<

# Level 4
var_definitions.o: var_definitions.F90 grid_module.o io_definitions.o ocean_var.o
	$(FC) $(FFLAGS) -c $<

# Level 5
io_manager.o: io_manager.F90 io_constants.o io_definitions.o io_config.o io_file_registry.o io_naming.o io_netcdf.o
	$(FC) $(FFLAGS) -c $<

# Level 6
var_registry.o: var_registry.F90 grid_module.o io_manager.o var_definitions.o ocean_var.o
	$(FC) $(FFLAGS) -c $<

# Level 7
main_test_output.o: main_test_output.F90 var_registry.o io_manager.o ocean_var.o grid_module.o
	$(FC) $(FFLAGS) -c $<

# Clean
clean:
	rm -f *.o *.mod $(TARGET)

cleanall: clean
	rm -f *.nc

# Run test
test: $(TARGET)
	./$(TARGET)
	@echo ""
	@echo "Checking output files..."
	@ls -la *.nc 2>/dev/null || echo "No NetCDF files found"

.PHONY: all clean cleanall test
