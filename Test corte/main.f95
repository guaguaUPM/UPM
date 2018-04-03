program main
use funciones
implicit none

real*8 :: x(2), tol

write(*,*) "Usando el modo bissecion, introduzca los dos puntos de contorno:"
read(*,*) x(1)
read(*,*) x(2)

write(*,*) "¿Que toleracia desea?"
read(*,*) tol

call corte_biseccion(xcubo,x(1),x(2),tol,corte)

write(*,*) corte

end program main