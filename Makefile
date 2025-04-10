NCDF_ROOT=/usr/local
NCDF_INC=$(NCDF_ROOT)/include
NCDF_LIB=$(NCDF_ROOT)/lib

FC = gfortran
FFLAGS = -O0 -g -Wall -fcheck=all -fbacktrace -I$(NCDF_INC)
LDFLAGS = -g -L$(NCDF_LIB) -lnetcdff -lnetcdf

# Original objects (parent modules)
MODULES = ocean_var.o namelist_output.o grid_module.o netcdf_backend.o file_manager.o variables_registry.o

# Submodules for file_manager
SUBMODULES = file_manager_buffers.o file_manager_registration.o file_manager_fileops.o \
             file_manager_writing.o file_manager_init.o

OBJS = $(MODULES) $(SUBMODULES) main_test_output.o

all: test_output.exe

%.o: %.F90
	$(FC) $(FFLAGS) -c $<

# Special rule for submodules
file_manager_%.o: file_manager_%.F90 file_manager.o
	$(FC) $(FFLAGS) -c $<

test_output.exe: $(OBJS)
	$(FC) -o $@ $(OBJS) $(LDFLAGS)

doc: test_output.exe
	ford ford.md

test:
	python ./verify_output.py

clean:
	rm -f *.nc *.o *.mod test_output.exe
