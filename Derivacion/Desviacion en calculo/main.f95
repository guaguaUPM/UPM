program main
use funciones
implicit none

real*8 :: m, x0, h
integer :: i

write(*,*) "En que punto desea la derivada?"
read(*,*) x0

open(unit=10,file='error.dat',status='unknown',action='write')
h = 1.d-14
do while (h >= 1.d-18)
    h=h-1.d-17
    write(*,*) h
    call derivada1_prog2(seno,x0,m,h)
    write(10,*) h, cos(x0)-m
end do
close(10)

open(unit=11,file='ref.dat',status='unknown',action='write')
write(11,*) 1.d-14, 0.d0
write(11,*) 1.d-18, 0.d0
close(11)

call SYSTEM("python plot11_derivada.py")

end program main