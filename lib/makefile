# source files.
SRC=$(wildcard *.f95)
 
OBJ = $(SRC:.f95=.o)
 
OUT = /usr/lib/libDIOS/libDIOS.a

# Fortran compiler flags
FFLAGS = -fbounds-check
 
# compiler
FC = gfortran
 
.SUFFIXES: .f95
 
default: dep $(OUT)
 
.f95.o:
	$(FC) $(FFLAGS) -c $< -o $@
 
$(OUT): $(OBJ)
	rm -f $(OUT)
	rm -f /usr/lib/libDIOS/*.mod
	ar -rcs $(OUT) $(OBJ)
	cp *.mod /usr/lib/libDIOS/
depend: dep
 
dep:
	gfortran -c $(SRC) $(FFLAGS)
