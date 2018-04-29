! Trabajo relizado por el Grupo 3, M6. 2018
! Participantes del grupo:
! · Fernando Ayats Llamas

program fractales
use newton
! El modulo de funciones solo se usa dentro de newton, por lo que no esta aqui

implicit none
integer               :: N, i, j                ! Dimension de la discretizacion; variables auxiliares
real*8, allocatable   :: x(:), y(:)             ! Vectores con los valores de la malla 
real*8                :: incremento, sol(2)     ! Distancia espacial entre nodos; parte real e imaginaria de la solucion

! Inicializacion de variables
write(*,*) "¿Que tamano desea?" 
read(*,*) N
allocate(x(N))
allocate(y(N))

! Malla de puntos de puntos iniciales
incremento = 2/(1.d0*(N-1))
do i = 0, N-1 
    x(i+1) = -1d0 + (i)*incremento
    y(i+1) =  1d0 - (i)*incremento
end do

OPEN(unit=10,file='valores.dat',status='unknown',action='write')

! Resolucion del problema z^3 - C = 0
do i = 1, N 
   do j = 1, N 
        call newton_raphson_2D(x(i), y(j), 1.d-6, 1000, sol)
        write(10,*) x(i), y(j), sol(1) + sol(2)
        ! Las soluciones del problema son: 
        ! 1: c^(1/3)
        ! 2: -(-1)^(1/3) c^(1/3)
        ! 3: (-1)^(2/3) c^(1/3)
        ! Sumando la parte real y la imaginaria, pueden distinguirse 
        ! las tres soluciones
    end do
end do

close(10)
deallocate(x)
deallocate(y)

! Automaticamente se muestran los resultados al usario
call SYSTEM("python plotARRAY.py")

end program fractales