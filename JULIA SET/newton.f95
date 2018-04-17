module newton
use lib_gauss
    implicit none

contains

subroutine corte_newton_raphson_sistemas(f1,f2, J, x, y, N, tol, max_iter, corte)
    real*8,intent(in)   :: x, y, tol 
    integer,intent(in)  :: max_iter, N
    real*8, intent(out) :: Corte(N)

    interface
        function f1(x, y)
            real*8, intent(in) :: x, y 
            real*8 :: f1
        end function
        function f2(x, y)
            real*8 :: f2
            real*8, intent(in)  :: x, y 
        end function
        function J(x, y)
            real*8, intent(in) :: x, y 
            real*8 :: J(2,2)
        end function
    end interface

    real*8 :: So(2), S1(2), normaS_So, normaS, Yn(N), BUFFER(2)
    integer :: i
    So(1) = x 
    S1(1) = x
    So(2) = y
    S1(2) = y


    do i = 1, max_iter
        
        BUFFER(1) = f1(So(1), So(2))
        BUFFER(2) = f2(So(1), So(2))  
        call resolver_GAUSS ( J(So(1),So(2)), BUFFER, Yn, N)
        S1 = So-Yn

        call norma2(normaS_So, S1-So, N)
        call norma2(normaS, S1, N)
        if (normaS_So/normaS <= tol) then
            Corte = S1
            return
          else
            So = S1
        end if
    enddo

end subroutine corte_newton_raphson_sistemas

subroutine norma2 (norma, vector, n)
    implicit none
    integer, intent(in)  :: n
    real(8), intent(in)  :: vector(n)
    real(8), intent(out) :: norma
    integer :: i
        
    norma = 0.d0
    do i = 1, n
        norma = norma+vector(i)**2
    end do
    norma = sqrt(norma) 
            
end subroutine norma2

end module newton