program main
use funciones
implicit none

real*8 :: m, x0

write(*,*) "En que punto desea la derivada?"
read(*,*) x0
call derivada1_cent1(arcotangente,x0,m)
write(*,*) "La derivada primera centrada vale:", m

call represtarR1_R1_tangente(arcotangente, -5.d0,5.d0, 30, x0,m)

end program main