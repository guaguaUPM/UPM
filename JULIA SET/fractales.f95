!
!////////////////////////////////////////////////////////////////////////
!
!      fractales.f90
!      Created: 20 April 2018 at 10:40 
!      By: Gonzalo Rubio  
!
!////////////////////////////////////////////////////////////////////////
!
PROGRAM fractales
use newton
IMPLICIT NONE 

!///////////////////////////
! Declaracion de variables ! 
!///////////////////////////

INTEGER :: N                                    ! Dimension de la discretizacion
REAL(8), ALLOCATABLE  :: x(:), y(:)             ! Vectores con los valores de la malla 
REAL(8)               :: incremento             ! Distancia espacial entre nodos
INTEGER               :: i,j                    ! Variable auxiliar
REAL(8)               :: sol(2)


! Inicializacion de variables
write(*,*) "¿Que tamano desea?" 
read(*,*) N
allocate(x(N))
allocate(y(N))

! Malla de puntos de puntos iniciales
incremento = 2/(1.d0*(N-1))
DO i = 0, N-1 
    x(i+1) = -1d0 + (i)*incremento
    y(i+1) = 1d0 - (i)*incremento
ENDDO 

OPEN(unit=10,file='valores.dat',status='unknown',action='write')

! Resolucion del problema z^3 - C = 0

DO i = 1, N 
   DO j = 1, N 
      CALL newton_raphson_2D(x(i), y(j), 1.d-4, 100, sol)
      WRITE(10,*) x(i), y(j), sol(1) + sol(2)
         ! Las soluciones del problema son: 
         ! 1: c^(1/3)
         ! 2: -(-1)^(1/3) c^(1/3)
         ! 3: (-1)^(2/3) c^(1/3)
         ! Sumando la parte real y la imaginaria, pueden distinguirse 
         !    las tres soluciones 
   ENDDO 
ENDDO 

CLOSE(10)


call SYSTEM("python plotARRAY.py")

END PROGRAM fractales   