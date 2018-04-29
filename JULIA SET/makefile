# Programa por defecto. Añadir modulos a la izquierda
SRC  = funciones.f95 newton.f95 fractales.f95

# Compilador fortran
FC = gfortran
# Argumentos de compilacion (Bounds para matrices y O3 para velocidad) 
#                                                    Descomentar para usar LAPACK/BLAS
FFLAGS = -O3 -L/usr/lib -llapack -L/usr/lib -lblas # -L$$HOME/libDIOS -lDIOS  

OBJ  = $(SRC:.f95=.o)
EXE  = run.out
.DEFAULT_GOAL := nolib
.SUFFIXES: .f95 .o
all: $(OBJ) main
nolib: $(OBJ) default

%.o : %.f95
	$(FC) $(FFLAGS) -c $<

# Desactivado para la entrga. Compila la liberia que usamos en los trabajos pero que aqui no hace falta
# main : $(OBJ)
# 	while [[ $$PWD != '/' && $${PWD##*/} != 'guaguaUPM' ]]; do cd ..; done; cd lib; make; cd ~-
# 	$(FC) $(OBJ) $(FFLAGS) -o $(EXE)

default : $(OBJ)
	$(FC) $(OBJ) $(FFLAGS) -o $(EXE)

clean:
	rm -f $(OBJ) *.mod
