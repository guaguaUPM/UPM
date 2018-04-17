program main
use funciones
use representar
use newton

implicit none
integer :: N, i, j
real*8, allocatable  :: CONDICION(:,:,:)
real*8 :: incremento, corte(2)
integer, allocatable :: COLOR(:,:)


! SE PIDEN LOS DATOS AL USUARIO
write(*,*) "¿Que tamano de matriz desea?"
read(*,*) N

allocate(CONDICION(N,N,2))
allocate(COLOR(N,N))

! SE CREA LA MATRIZ DE CONDICIONES INICIALES
incremento = 2/(1.d0*(N-1))
do i = 0, N-1
    CONDICION(:,i+1,1) = -1d0 + (i)*incremento
    CONDICION(i+1,:,2) = 1d0 - (i)*incremento
end do

!write(*,*) "Coordenadas X:"
!call write_A(CONDICION(:,:,1),N)
!write(*,*) "Coordenadas Y:"
!call write_A(CONDICION(:,:,2),N)


do i=1, N
    do j=1, N
        call corte_newton_raphson_sistemas(F, jacobiano, CONDICION(i,j,1), CONDICION(i,j,2), 2, 10.d-7, 500, corte)
        if (abs(corte(2)) < 10.d-5) then
            COLOR(i,j) = 1
        else if (corte(2) > 0.d0 ) then
            COLOR(i,j) = 2
        else
            COLOR(i,j) = 3
        end if
    end do
end do

!call write_A_int(COLOR,N)
call array_a_colores(COLOR,N)

deallocate(CONDICION)
end program main