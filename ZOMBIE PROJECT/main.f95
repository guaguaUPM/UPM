program main
use zombies
use euler
implicit none

real*8 :: X0(2)

X0(1) = 0.d0
X0(2) = 0.d0

call resolver_EDO(s_prima, z_prima, r_prima,200, 500.d0, 0.d0, 0.d0, 0.d0, 2.5d0)

end program main