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
    real*8  :: X, m

    X = X0
    do i = 1, N
