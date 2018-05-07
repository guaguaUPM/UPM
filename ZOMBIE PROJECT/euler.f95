module euler
contains
subroutine resolver_EDO(DERIVADA,N,X0,Yfinal)
    implicit none
    integer, intent(in) :: N
    real*8, intent(in)  :: X0(2), Yfinal
    interface
        function DERIVADA(X)
        real*8 :: X
        real*8 :: DERIVADA
        end function
    end interface

    integer :: i
    real*8  :: X(2), incremento

    incremento = (Yfinal - X0(1)) / (N*1.d0)

    open(unit=10,file='valores.dat',status='unknown',action='write')
    X = X0
    write(10,*) X
    write(*,*) X, incremento, "H"
    do i = 1, N+1
         
        !write(*,*) DERIVADA(X(1)), DERIVADA(X(1))*incremento, X(2), "COS", cos(x(1))
        X(2) = DERIVADA(X(1)) * incremento + X(2)
        X(1) = X(1) + incremento
        write(10,*) X
        write(*,*) X

    end do
    close(10)
end subroutine
end module
