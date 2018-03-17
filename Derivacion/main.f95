program main
use funciones
implicit none

real*8 :: m, x0

write(*,*) "En que punto desea la derivada?"
read(*,*) x0
call derivada1_cent1(f001,x0,m)
write(*,*) "La derivada primera centrada vale:", m

call represtarR1_R1_tangente(f001, -20.d0,20.d0, 80, x0,m)

end program main