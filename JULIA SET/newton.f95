module newton
implicit none

contains

subroutine corte_newton_raphson_sistemas(f, J, x, y, N, tol, max_iter, corte)
    real*8,intent(in)   :: x, y, N, tol 
    integer,intent(in)  :: max_iter
    real*8, intent(out) :: Corte(N)

    interface
        function f(x, y)
            real*8 :: x, y 
            real*8 :: f(2)
        end function
        function J(x, y)
            real*8 :: x, y 
            real*8 :: J(2,2)
        end function
    end interface

    real*8 :: So(2), S1(2), normaS-So, normaS, Y(N)
    So(1) = x 
    S1(1) = x
    So(2) = y
    S1(2) = y


    do iter = 1, max_iter
        
        call resolver_LAPACK (J(So(1),So(2)), f(So(1), So(2)), Y, N)
        S1 = So-Y

        call norma2(normaS-So, S1-So, N)
        call norma2(normaS, S, N)
        if (normaS-So/normaS <= tol) then
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