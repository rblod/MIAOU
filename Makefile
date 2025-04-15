NCDF_ROOT=/usr/local
NCDF_INC=$(NCDF_ROOT)/include
NCDF_LIB=$(NCDF_ROOT)/lib

FC = gfortran
FFLAGS = -O0 -g -Wall -fcheck=all -fbacktrace -I$(NCDF_INC)
LDFLAGS = -g -L$(NCDF_LIB) -lnetcdff -lnetcdf

# Core modules - independent of backend
CORE_MODULES = ocean_var.o grid_module.o netcdf_utils.o netcdf_backend.o

# I/O System modules - new architecture
IO_MODULES = io_definitions.o io_config.o io_netcdf.o io_netcdf_avg.o io_manager.o

# Variable definition and registration modules
VAR_MODULES = var_definitions.o var_registry.o

# All modules combined
MODULES = $(CORE_MODULES) $(IO_MODULES) $(VAR_MODULES)

# Object files
OBJS = $(MODULES) main_test_output.o

all: test_output.exe

%.o: %.F90
	$(FC) $(FFLAGS) -c $<

# Special rule for file_manager submodules
file_manager_%.o: file_manager_%.F90 file_manager.o
	$(FC) $(FFLAGS) -c $<

# Build order dependency rules for new architecture
io_config.o: io_definitions.o
io_netcdf.o: io_definitions.o io_config.o netcdf_utils.o netcdf_backend.o
io_netcdf_avg.o: io_definitions.o ocean_var.o
io_manager.o: io_definitions.o io_config.o io_netcdf.o

var_definitions.o: grid_module.o io_definitions.o ocean_var.o
var_registry.o: grid_module.o io_definitions.o io_manager.o var_definitions.o ocean_var.o


test_output.exe: $(OBJS)
	$(FC) -o $@ $(OBJS) $(LDFLAGS)

doc: test_output.exe
	ford ford.md

test:
	python ./verify_output.py

clean:
	rm -f *.nc *.o *.smod *.mod test_output.exe

# Show module dependencies (for debugging)
deps:
	@echo "Module dependencies:"
	@echo "Core modules: $(CORE_MODULES)"
	@echo "I/O modules: $(IO_MODULES)"
	@echo "Variable modules: $(VAR_MODULES)" 
	@echo "File manager modules: $(FILE_MANAGER_MODS)"