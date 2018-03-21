program main
use funciones
implicit none

real*8 :: m, x0

write(*,*) "En que punto desea la derivada?"
read(*,*) x0
call derivada1_prog2(seno,x0,m,10.d-7)
write(*,*) "Valor: ", m
write(*,*) "Exacto:", cos(x0)

! call represtarR1_R1_tangente(seno, -3.d0,3.d0, 80, x0,m)

end program main