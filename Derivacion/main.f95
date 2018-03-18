program main
use funciones
implicit none

real*8 :: m, x0

write(*,*) "En que punto desea la derivada?"
read(*,*) x0
call derivada1_prog1(parabola,x0,m,SQRT(EPSILON(X0)))
write(*,*) "La derivada primera centrada vale:", m

call represtarR1_R1_tangente(parabola, -3.d0,3.d0, 80, x0,m)

end program main