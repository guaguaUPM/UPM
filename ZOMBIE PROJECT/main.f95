program main
use funciones
use euler
implicit none

real*8 :: X0(2)

X0(1) = 0
X0(2) = 1

call resolver_EDO(exponencial,50,X0, 5.d0)

end program main