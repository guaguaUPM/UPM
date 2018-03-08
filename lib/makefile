# source files.
SRC=$(wildcard *.f95)
 
OBJ = $(SRC:.f95=.o)
 
OUT = $$HOME/libDIOS/libDIOS.a

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
	ar -rcs $(OUT) $(OBJ)
depend: dep
 
dep:
	gfortran -c $(SRC) $(FFLAGS)
