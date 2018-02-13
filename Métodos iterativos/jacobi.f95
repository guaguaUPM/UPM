module metodos_iterativos

contains

    subroutine jacobi (A, Xfinal, b)

        ! Argumentos de la subrutina
        real(8), intent(in) :: A(:,:)     !
        real(8), intent(in) :: b(:)       ! DIMENSIONES ASUMIDAS
        real(8), intent(inout) :: Xfinal(:)    !

        ! Variables locales
        integer :: n                     ! Dimensión del problema A(n,n) b(n) X(n)
        real(8), allocatable :: x0(:)
        real(8), allocatable :: x(:)
        integer :: iter, j, k, maxiter

        n = size(A,1)
        allocate (x0(n))
        allocate (x(n))

        do iter = 1, maxiter


        end do

    end subroutine

    function norma2 (vector,n)
        real(8), intent(in) :: vector(:)
        integer, intent(in) :: n
        real(8) :: norma2

        norma2 = 0.d0
        do i = 1, n
            norma2 = norma2+vector(i)**2
        enddo
        norma2 = sqrt(norma2) 
    
    end function

end module