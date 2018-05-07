program main
use funciones
use euler
implicit none

real*8 :: X0(2)

X0(1) = 0.d0
X0(2) = 0.d0

call resolver_EDO(coseno,500,X0, 5.d0)

end program main