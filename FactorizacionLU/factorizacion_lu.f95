module factorizacion_lu
contains

subroutine factorizar(ENTRADA, L, U, TAMANO)
    implicit none

    ! La matriz entrada no debe tener ceros en la diagonal

    real*8, intent(in)   ::  ENTRADA(TAMANO, TAMANO)
    real*8, intent(out)  ::  L(TAMANO, TAMANO), U(TAMANO, TAMANO)
    integer, intent(in)  ::  TAMANO
    integer              :: i, j

    write(*,*) "1"

    U=-1.d0
    L=-1.d0

    do i = 1, TAMANO
        U(i,i) = 1.d0
        do j = i+1, TAMANO
            U(i,j) = 0.d0
        end do
        do j = 0, i-1
            L(i,j) = 0.d0
        end do
    end do

end subroutine factorizar
end module factorizacion_lu