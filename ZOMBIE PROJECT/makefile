# Programa por defecto. Añadir modulos a la izquierda
SRC  = files.f95 zombies.f95 euler.f95  main.f95

# Compilador fortran
FC = gfortran
# Argumentos de compilacion (Bounds para matrices y O3 para velocidad) 
#                                                    Descomentar para usar LAPACK/BLAS
FFLAGS = -fbounds-check #-O4 #  -L$$HOME/libDIOS -lDIOS# -L/usr/lib -llapack -L/usr/lib -lblas

OBJ  = $(SRC:.f95=.o)
EXE  = run.out
.DEFAULT_GOAL := nolib
.SUFFIXES: .f95 .o
#all: $(OBJ) main
nolib: $(OBJ) default

%.o : %.f95
	$(FC) $(FFLAGS) -c $<

#main : $(OBJ)
#	while [[ $$PWD != '/' && $${PWD##*/} != 'guaguaUPM' ]]; do cd ..; done; cd lib; make; cd ~-
#	$(FC) $(OBJ) $(FFLAGS) -o $(EXE)

default : $(OBJ)
	$(FC) $(OBJ) $(FFLAGS) -o $(EXE)

clean:
	rm -f $(OBJ) *.mod
