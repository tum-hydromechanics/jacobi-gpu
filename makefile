
FC=mpiifort -fc=ifx
CC=mpiicpc -cxx=icpx

# flags to run on GPU
FFLAGS=-c -g -fiopenmp  -fopenmp-targets=spir64 -qopt-report=3 -O3 #  -traceback -check bounds
LFLAGS=-g -fiopenmp -fopenmp-targets=spir64 -qopt-report=3 -O3 #  -traceback -check bounds

mglet: main.o pseudo_connect_mod.o
	$(FC) -fopenmp main.o pseudo_connect_mod.o $(LFLAGS) -o main.exe

main.o: main.F90 pseudo_connect_mod.o
	$(FC) $(FFLAGS) main.F90

pseudo_connect_mod.o: pseudo_connect_mod.F90
	$(FC) $(FFLAGS) pseudo_connect_mod.F90



# Clean current directory, but leave executable file
.PHONY: clean
clean:
	rm -f *.o *.f *.f90 *.mod *.s *~ *.exe *_t1 *.opt.yaml *opt* *.modmic
