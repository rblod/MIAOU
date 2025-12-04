# MIAOU Makefile
# Modular I/O Architecture for Ocean and Unified models

FC = gfortran
FFLAGS = -g -Wall -Wextra -fcheck=all -fbacktrace -O0 -cpp

# NetCDF configuration
NCDF_INC = $(shell nf-config --fflags 2>/dev/null || echo "-I/usr/include")
NCDF_LIB = $(shell nf-config --flibs 2>/dev/null || echo "-lnetcdff -lnetcdf")

FFLAGS += $(NCDF_INC)
LDFLAGS = $(NCDF_LIB)

# Source files by dependency level
SRCS_L0 = io_constants.F90 io_error.F90 grid_module.F90 ocean_var.F90
SRCS_L1 = io_naming.F90 netcdf_utils.F90 io_definitions.F90
SRCS_L2 = netcdf_backend.F90 io_file_registry.F90
SRCS_L3 = io_netcdf.F90 io_config.F90
SRCS_L4 = io_manager.F90
SRCS_L5 = var_definitions.F90
SRCS_L6 = var_registry.F90
SRCS_L7 = main_test_output.F90

SRCS = $(SRCS_L0) $(SRCS_L1) $(SRCS_L2) $(SRCS_L3) $(SRCS_L4) $(SRCS_L5) $(SRCS_L6) $(SRCS_L7)
OBJS = $(SRCS:.F90=.o)
TARGET = test_output.exe

all: $(TARGET)

$(TARGET): $(OBJS)
	$(FC) $(FFLAGS) -o $@ $(OBJS) $(LDFLAGS)

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
io_manager.o: io_manager.F90 io_constants.o io_definitions.o io_config.o io_file_registry.o io_naming.o io_netcdf.o
	$(FC) $(FFLAGS) -c $<

# Level 5
var_definitions.o: var_definitions.F90 grid_module.o io_definitions.o ocean_var.o io_manager.o output_vars.inc
	$(FC) $(FFLAGS) -c $<

# Level 6
var_registry.o: var_registry.F90 grid_module.o io_manager.o var_definitions.o ocean_var.o
	$(FC) $(FFLAGS) -c $<

# Level 7
main_test_output.o: main_test_output.F90 var_registry.o io_manager.o ocean_var.o grid_module.o var_definitions.o
	$(FC) $(FFLAGS) -c $<

clean:
	rm -f *.o *.mod $(TARGET)

cleanall: clean
	rm -f *.nc

test: $(TARGET)
	./$(TARGET)
	@echo ""
	@ls -la *.nc 2>/dev/null || echo "No NetCDF files found"

.PHONY: all clean cleanall test
