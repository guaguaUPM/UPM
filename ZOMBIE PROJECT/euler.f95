module euler
contains
subroutine resolver_EDO(DERIVADA,N,X0)
    implicit none
    integer, intent(in) :: N
    real*8, intent(in)  :: X0(2)
    interface
        function DERIVDA(X)
        real*8 :: X
        real*8 :: DERIVADA
        end function
    end interface

    integer :: i
    real*8  :: X(2), m, incremento

    incremento = (X0(2) - X0(1)) / (N*1.d0)

    open(unit=10,file='valores.dat',status='unknown',action='write')
    X = X0
    do i = 1, N
        m = DERIVADA(X(1))

        X(2) = m * X(1) + X(2)
        X(1) = X(1) + incremento

        write(*,*) X
        write(10,*) X
    end do
    close(10)
end subroutine
end module
