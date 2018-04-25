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

use funciones
use newton
use lib_gauss
IMPLICIT NONE 

!///////////////////////////
! Declaracion de variables ! 
!///////////////////////////

INTEGER :: N                                    ! Dimension de la discretizacion
!REAL(8), ALLOCATABLE  :: x(:), y(:)             ! Vectores con los valores de la malla 
real*8, allocatable  :: CONDICION(:,:,:)
REAL(8)               :: incremento                     ! Distancia espacial entre nodos
INTEGER               :: i,j                    ! Variable auxiliar

REAL(8)               :: sol(2)


! Inicializacion de variables
N         = 100
incremento = 2/(1.d0*(N-1))

! Reserva de memoria
allocate(CONDICION(N,N,2))

! Inicializacion de variables a cero

!DO i = 1, N 
!    x(i) = -1d0 + (i-1) * 2 * dx  
!    y(i) = -1d0 + (i-1) * 2 * dx  
!ENDDO 
do i = 0, N-1
    CONDICION(:,i+1,1) = -1d0 + (i)*incremento
    CONDICION(i+1,:,2) = 1d0 - (i)*incremento
end do

OPEN(UNIT=10,FILE="file.dat")

! Resolucion del problema z^3 - C = 0

DO i = 1, N 
   DO j = 1, N 
   
      sol(1) = CONDICION(i,j,1)
      sol(2) = CONDICION(i,j,2)
      CALL corte_newton_raphson_sistemas(FUNCION1,FUNCION2, Jacobiano, CONDICION(i,j,1), CONDICION(i,j,2), 2, 1.d-2, 1000, sol)
      
      WRITE(10,*) CONDICION(i,j,1), CONDICION(i,j,2), sol(1)+sol(2)
         ! Las soluciones del problema son: 
         ! 1: c^(1/3)
         ! 2: -(-1)^(1/3) c^(1/3)
         ! 3: (-1)^(2/3) c^(1/3)
         ! Sumando la parte real y la imaginaria, pueden distinguirse 
         !    las tres soluciones 

   ENDDO 
ENDDO 

CLOSE(10)

END PROGRAM fractales   