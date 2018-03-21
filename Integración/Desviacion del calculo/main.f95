program main
use funciones
implicit none

real*8 :: inicio, fin, area, VALOR_REAL
integer :: i, partINICIO, partFINAL, salto

write(*,*) "¿Cual es el intervalo bajo el que calcular la integral?"
read(*,*) inicio
read(*,*) fin

partINICIO = 30
partFINAL = 500
salto = 1

! SQRT(PI)
VALOR_REAL =sqrt( 4.D0*DATAN(1.D0) )

open(unit=10,file='errorR.dat',status='unknown',action='write')
open(unit=11,file='errorT.dat',status='unknown',action='write')
open(unit=12,file='errorS.dat',status='unknown',action='write')

do i = partINICIO, partFINAL, salto
    call integral_riemann(campana,inicio,fin,i,area)
    write(10,*) i, VALOR_REAL - area
    call integral_trapcio(campana,inicio,fin,i,area)
    write(11,*) i, VALOR_REAL - area
    call integral_simpson(campana,inicio,fin,i,area)
    write(12,*) i, VALOR_REAL - area
end do

close(10)
close(11)
close(12)

!open(unit=11,file='ref.dat',status='unknown',action='write')
!write(11,*) 10.d0**(expINICIO), 0.d0
!write(11,*) 10.d0**(expFINAL), 0.d0
!close(11)

call SYSTEM("python plot11_custom.py")

end program main