program main
use funciones
implicit none

real*8 :: x(2), tol, corte

write(*,*) "FUNCION:  x^3 - e"
! -------------
write(*,*) "Usando el modo bissecion, introduzca los dos puntos de contorno:"
read(*,*) x(1)
read(*,*) x(2)
write(*,*) "¿Que toleracia desea?"
read(*,*) tol

call corte_biseccion(principal,x(1),x(2),tol,corte)
write(*,*) "RESULTADO:", corte

write(*,*)
! --------------
write(*,*) "Usando el metodo de newton con derivada analitica, introduzca el punto de partida:"
read(*,*) x(1)
write(*,*) "Usando la misma tolerancia que antes..."
! read(*,*) tol

call corte_newton_anal(principal, secundaria, x(1), tol, corte)
write(*,*) "RESULTADO:", corte

end program main