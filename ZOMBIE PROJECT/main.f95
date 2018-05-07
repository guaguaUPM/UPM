program main
use funciones
use euler
implicit none

real*8 :: X0(2)

X0(1) = 1
X0(2) = 2

call resolver_EDO(exponencial,500,X0)

end program main