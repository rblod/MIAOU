NCDF_ROOT=/usr/local
NCDF_INC=$(NCDF_ROOT)/include
NCDF_LIB=$(NCDF_ROOT)/lib

FC = gfortran
FFLAGS = -O0 -g  -Wall -fcheck=all -fbacktrace -I$(NCDF_INC)
LDFLAGS = -g -L$(NCDF_LIB) -lnetcdff -lnetcdf

OBJS =  ocean_var.o namelist_output.o grid_module.o file_manager.o variables_registry.o main_test_output.o

all: test_output.exe

%.o: %.F90
	$(FC) $(FFLAGS) -c $<

test_output.exe: $(OBJS)
	$(FC) -o $@ $(OBJS) $(LDFLAGS)

clean:
	rm -f *.o *.mod test_output.exe
