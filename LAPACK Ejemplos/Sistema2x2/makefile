# FORTRAN compiler given below
FC = gfortran

# LINKER GIVEN BELOW
LD = gfortran

# COMPILER FLAGS GIVEN BELOW
FFLAGS  = -fbounds-check -L/usr/lib -llapack -L/usr/lib -lblas

# COMMAND TO DELETE (Windows=del, MacOS=rm)
RM = rm

SRC  = ascii_lapack.f95 main.f95 

OBJ  = $(SRC:.f95=.o)
EXE  = run.out

.SUFFIXES: .f95 .o

all: $(OBJ) main

%.o : %.f95
	$(FC) $(FFLAGS) -c $<

main : $(OBJ)
	$(LD) $(FFLAGS) $(OBJ) -o $(EXE)

clean:
	$(RM) -f $(OBJ) *.mod
