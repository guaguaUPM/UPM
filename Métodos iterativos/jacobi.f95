module metodos_iterativos

contains

    subroutine jacobi (A, X, b)

        ! Argumentos de la subrutina
        real(8), intent(in) :: A(:,:)     !
        real(8), intent(in) :: b(:)       ! DIMENSIONES ASUMIDAS
        real(8), intent(inout) :: x(:)    !

        ! Variables locales
        integer :: m                     ! Dimensión del problema A(m,m) b(m) X(m)
        real(8), allocatable :: porsi(:,:)
        real(8) :: h
        integer :: i,j,k

        m = size(A,1)





    end subroutine

    function norma2 (vector,n)
        real(8), intent(in) :: vector(:)
        real(8), intent(in) :: n
        real(8) :: norma2

        norma2 = 0.d0
        do i = 1,n
            norma2 = norma2+vector(i)**2
        enddo
        norma2 = sqrt(norma2) 
    
    end function

end module