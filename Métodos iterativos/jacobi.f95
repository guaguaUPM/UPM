module metodos_iterativos
use invertir

contains

    subroutine jacobi (A, Xfinal, b)

        ! Argumentos de la subrutina
        real(8), intent(in) :: A(:,:)          !
        real(8), intent(in) :: b(:)            ! DIMENSIONES ASUMIDAS
        real(8), intent(inout) :: Xfinal(:)    !

        ! Variables locales
        integer :: n                     ! Dimensión del problema A(n,n) b(n) X(n)
        real(8), allocatable :: x0(:)
        real(8), allocatable :: x(:)
        real(8), allocatable :: L(:,:)
        real(8), allocatable :: D(:,:)
        real(8), allocatable :: U(:,:)
        integer :: iter, j, k, maxiter

        n = size(A,1)
        allocate (x0(n))
        allocate (x(n))
        allocate (L(n,n))
        allocate (D(n,n))
        allocate (U(n,n))

        !Primera semilla (primer valor establecido de x para iniciar la iteración)
        x0 = 0.d0


        !Establecemos las matrices L, D y U para el algoritmo de la iteración
        do j= 1, n
            do k=1, n
                if (k > j) then
                    U(j,k) = A(j,k)
                else if (k == j)
                    D(j,k) = A(j,k)
                else 
                    L(j,k) = A(j,k)
                end if
            end do
        end do
        
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