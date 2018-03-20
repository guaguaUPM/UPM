program main
use funciones
implicit none

real*8 :: m, x0

write(*,*) "En que punto desea la derivada?"
read(*,*) x0
call derivada1_prog1(seno,x0,m,1.d-7)
write(*,*) "La derivada primera centrada vale:", m
write(*,*) "El valor exacto es:", cos(x0)

call represtarR1_R1_tangente(seno, -3.d0,3.d0, 80, x0,m)

end program main