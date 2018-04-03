subroutine datos_a_puntos(VECTOR,INCREMENTO,X0,N)
    implicit none
    integer, intent(in) :: N
    real*8, intent(in) :: VECTOR(N), INCREMENTO, X0

    real*8 :: x
    integer :: i

    x = X0

    open(unit=10,file='puntos.dat',status='unknown',action='write')
    do i = 1, N
        write(10,*) x, VECTOR(N)
        x = x + INCREMENTO
    end do
    close(10)
end subroutine datos_a_puntos