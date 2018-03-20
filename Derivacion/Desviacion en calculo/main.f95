program main
use funciones
implicit none

real*8 :: m, x0, h, epsi
integer :: i

write(*,*) "En que punto desea la derivada?"
read(*,*) x0
epsi = EPSILON(x0)
open(unit=10,file='error.dat',status='unknown',action='write')
h = 1.d-5
do i = -5, -16, -1
    h=10.d0**(i)
    write(*,*) h
    call derivada1_cent2(seno,x0,m,h)
    write(*,*) cos(x0), m, cos(x0)-m
    write(10,*) h, cos(x0)-m
end do
close(10)

open(unit=11,file='ref.dat',status='unknown',action='write')
write(11,*) 1.d-5, 0.d0
write(11,*) 1.d-16, 0.d0
close(11)

call SYSTEM("python plot11_derivada.py")

end program main