FC=gfortran
FFLAGS=-O3 -Wall -Wextra -fdefault-real-8
SRC= mem.f90 accel.f90 eos.f90 ghosts.f90 utils.f90 density.f90 derivs.f90 step.f90 main.f90
OBJ=${SRC:.f90=.o}


%.o: %.f90
		$(FC) $(FFLAGS) -o $@ -c $<

run: $(OBJ)
	$(FC) $(FFLAGS) -o $@ $(OBJ)

clean:
	rm *.o *.mod *.dat *snap*
