module newton
!use lib_gauss
implicit none

contains

subroutine corte_newton_raphson_sistemas(f1,f2, J, x0, y0, tol, max_iter, corte)
    real*8,intent(in)   :: x0, y0, tol 
    integer,intent(in)  :: max_iter
    real*8, intent(out) :: corte(2)
    real*8              :: x, y, corte_anterior(2)
    integer             :: IPIV(2,2), info
    real*8 :: normaA, normaB, BUFFER(2)
    integer :: i

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

    corte(1) = x0
    corte(2) = y0

    do i = 1, max_iter
        
        ! Se resuelve J(Xi) * M = F(Xi)
        BUFFER(1) = f1(corte(1),corte(2))
        BUFFER(2) = f2(corte(1),corte(2))
        ! Con DGESV BUFFER pasa de ser F(Xi) la solucion, por lo que solo se usa un vector y una matriz
        call DGESV(2,1, J(x,y) ,2,IPIV, BUFFER ,2,info)

        corte_anterior = corte
        corte = corte - BUFFER

        call norma2(normaA, corte-corte_anterior, 2)
        call norma2(normaB, corte, 2)
        if (normaA/normaB <= tol) then
            exit
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