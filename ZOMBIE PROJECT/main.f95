program main
use zombies
use euler
implicit none


call resolver_EDO(s_prima, z_prima, r_prima,200, 500.d0, 0.d0, 0.d0, 0.d0, 2.5d0)

end program main