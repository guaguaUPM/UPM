subroutine datos_a_puntos(VECTOR,N)
    implicit none
    integer, intent(in) :: N
    real*8, intent(in) :: VECTOR(N)

    integer :: i


    open(unit=10,file='puntos.dat',status='unknown',action='write')
    do i = 1, N
        write(10,*) Vector(N), i
    end do
    close(10)
end subroutine datos_a_puntos