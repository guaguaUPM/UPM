module factorizacion_lu
contains

subroutine factorizar(ENTRADA, L, U, TAMANO)
    implicit none

    ! La matriz entrada no debe tener ceros en la diagonal

    real*8, intent(in)   ::  ENTRADA(:,:)
    real*8, intent(out), allocatable   ::  L(:,:), U(:,:)
    real*8, allocatable  :: m(:,:)
    integer, intent(in)  ::  TAMANO
    integer              :: i, j

    write(*,*) TAMANO
    allocate(L(tamano,tamano))
    allocate(U(tamano,tamano))
    allocate(m(tamano,tamano))
    write(*,*) "1" !! DEBUG

    U=-1.d0
    L=-1.d0

    ! Se asignan los valores por defecto a las matrices (1 en la diagonal de U)
    do i = 1, TAMANO
        U(i,i) = 1.d0

        do j = i+1, TAMANO
            L(i,j) = 0.d0
        end do
        do j = 1, i-1
            U(i,j) = 0.d0
        end do
    end do
    L(1,1) = ENTRADA(1,1)

    do j=1, tamano-1
        do i=j+1, tamano

            M(i,j) = U(i,j)/J(j,j)
            U(i,:) = U(i,:) - m(i,j) * U(j,:)
        end do
    end do


end subroutine factorizar
end module factorizacion_lu