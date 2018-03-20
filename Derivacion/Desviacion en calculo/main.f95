program main
use funciones
implicit none

real*8 :: m, x0, h
integer :: i, expINICIO, expFINAL, particiones

write(*,*) "En que punto desea la derivada?"
read(*,*) x0

expINICIO = -14
expFINAL = -17
particiones = 100


open(unit=10,file='error.dat',status='unknown',action='write')
do i = 1, particiones
    h=10.d0**( (expFINAL-expINICIO)/(particiones*1.d0) * (i-1) + expINICIO )
    write(*,*) h
    call derivada1_cent2(seno,x0,m,h)
    write(*,*) cos(x0), m, cos(x0)-m
    write(10,*) h, cos(x0)-m
end do
close(10)

open(unit=11,file='ref.dat',status='unknown',action='write')
write(11,*) 10.d0**(expINICIO), 0.d0
write(11,*) 10.d0**(expFINAL), 0.d0
close(11)

call SYSTEM("python plot11_derivada.py")

end program main