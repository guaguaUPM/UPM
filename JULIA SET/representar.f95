module representar
contains

subroutine array_a_colores(M,N)
    implicit none
    integer, intent(in) :: N
    real*8, intent(in)  :: M(N,N)
    integer :: i

    open(unit=10,file='valores.dat',status='unknown',action='write')
    do i=1, N
        write(10,*) M(i,:)
    end do
    close(10)
end subroutine array_a_colores

end module representar