program movedor
implicit none

real*8, allocatable :: PUNTO(:,:), PUNTOmod(:,:)
integer :: i, TAMANO

open(unit=10, file='puntos.raw.dat', status='old', action='read')
TAMANO = 0
do
    read(10,*,END=69)
    TAMANO = TAMANO + 1
end do
69 CONTINUE
allocate(PUNTO(TAMANO,2))
rewind 10

! Se leen los valores de los puntos
do i=1, TAMANO
    read(10,*) PUNTO(i,1), PUNTO(i,2)
end do

close(10) ! Se cierra para evitar memory leaks

PUNTO(:,1) = PUNTO(:,1) + 273.d0
open(unit=12,file='puntos.rawK.dat',status='unknown',action='write')
do i=1, TAMANO
    write(12,*) PUNTO(i,:)
end do
close(12)

open(unit=11,file='puntos.mod.dat',status='unknown',action='write')
do i=1, TAMANO
    write(11,*) 1.d0/(PUNTO(i,1)*1.d0) , log(PUNTO(i,2)*1.d0)
end do
close(11)

end program movedor