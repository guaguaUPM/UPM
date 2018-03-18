program main
use funciones
implicit none

real*8 :: m, x0

write(*,*) "En que punto desea la derivada?"
read(*,*) x0
call derivada1_prog2(seno,x0,m,1.0d-15)
write(*,*) "La derivada primera centrada vale:", m

call represtarR1_R1_tangente(seno, -3.d0,3.d0, 80, x0,m)

end program main