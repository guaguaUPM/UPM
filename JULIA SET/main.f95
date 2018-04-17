program main
use funciones
use representar

implicit none
integer :: N, i
real*8, allocatable  :: CONDICION(:,:,:)
real*8 :: incremento


! SE PIDEN LOS DATOS AL USUARIO
write(*,*) "¿Que tamano de matriz desea?"
read(*,*) N

allocate(CONDICION(N,N,2))

! SE CREA LA MATRIZ DE CONDICIONES INICIALES
incremento = 2/(1.d0*(N-1))
do i = 0, N-1
    CONDICION(:,i+1,1) = -1d0 + (i)*incremento
    CONDICION(i+1,:,2) = 1d0 - (i)*incremento
end do
if(N <= 7) then
write(*,*) "Coordenadas X:"
call write_A(CONDICION(:,:,1),N)

write(*,*) "Coordenadas Y:"
call write_A(CONDICION(:,:,2),N)
end if

call array_a_colores(CONDICION(:,:,1), N)

deallocate(CONDICION)
end program main