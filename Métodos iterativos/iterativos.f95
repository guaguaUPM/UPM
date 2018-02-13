module metodos_iterativos
use invertir

!REQUIERE QUE EL SISTEMA CONVERJA
!REQUIERE QUE NO EXISTAN CEROS EN LA DIAGONAL (PREVIO PIVOTAMIENTO)

contains

    subroutine jacobi (A, Xfinal, b, tol)

        ! Argumentos de la subrutina
        real(8), intent(in) :: A(:,:)          !
        real(8), intent(in) :: b(:,:)            ! DIMENSIONES ASUMIDAS
        real(8), intent(inout) :: Xfinal(:)    !
        real(8), intent(in) :: tol             !

        ! Variables locales
        integer :: n                     ! Dimensión del problema A(n,n) b(n) X(n)
        real(8), allocatable :: x0(:)
        real(8), allocatable :: x(:)
        real(8), allocatable :: L(:,:)
        real(8), allocatable :: D(:,:)
        real(8), allocatable :: U(:,:)
        real(8), allocatable :: c(:,:)
        real(8), allocatable :: T(:,:)
        integer :: iter, j, k, maxiter

        n = size(A,1)
        allocate (x0(n))
        allocate (x(n))
        allocate (L(n,n))
        allocate (D(n,n))
        allocate (U(n,n))
        allocate (c(n,n))
        allocate (T(n,n))

        !Establecemos las matrices L, D y U para el algoritmo de la iteración
        do j= 1, n
            do k=1, n
                if (k > j) then
                    U(j,k) = A(j,k)
                else if (k == j) then
                    D(j,k) = A(j,k)
                else 
                    L(j,k) = A(j,k)
                end if
            end do
        end do
        
        !Segun nos indica la formula reducida del método de Jacobi calculamos las matrices c y T
        c = matmul(b, inversa(D)) !CAMBIO DE ORDEN
        T = matmul((-1)*inversa(D),(U+L))

        !Primera semilla (primer valor establecido de x para iniciar la iteración)
        x0 = 0.d0
        maxiter = 999999

        do iter = 1, maxiter
            x = matmul(x0, T) !CAMBIO DE ORDEN
            if (norma2((x-x0), n)/norma2(x, n) <= tol) then
                Xfinal = x
                stop
            else 
                x0 = x
            end if
        end do

    end subroutine

    subroutine gauss_seidel (A, Xfinal, b, tol)
        ! Argumentos de la subrutina
        real(8), intent(in) :: A(:,:)          !
        real(8), intent(in) :: b(:)            ! DIMENSIONES ASUMIDAS
        real(8), intent(inout) :: Xfinal(:)    !
        real(8), intent(in) :: tol             !
    
        ! Variables locales
        integer :: n                     ! Dimensión del problema A(n,n) b(n) X(n)
        real(8), allocatable :: x0(:)
        real(8), allocatable :: x(:)
        real(8), allocatable :: L(:,:)
        real(8), allocatable :: D(:,:)
        real(8), allocatable :: U(:,:)
        real(8), allocatable :: c(:,:)
        real(8), allocatable :: T(:,:)
        integer :: iter, j, k, maxiter
    
        n = size(A,1)
        allocate (x0(n))
        allocate (x(n))
        allocate (L(n,n))
        allocate (D(n,n))
        allocate (U(n,n))
        allocate (c(n,n))
        allocate (T(n,n))
    
        !Establecemos las matrices L, D y U para el algoritmo de la iteración
        do j= 1, n
            do k=1, n
                if (k > j) then
                    U(j,k) = A(j,k)
                else if (k == j) then
                    D(j,k) = A(j,k)
                else 
                    L(j,k) = A(j,k)
                end if
            end do
        end do
            
        !Segun nos indica la formula reducida del método de Jacobi calculamos las matrices c y T
        !c = matmul(inversa(D+L), b)
        !T = matmul((-1)*inversa(D+L), U)
    
        !Primera semilla (primer valor establecido de x para iniciar la iteración)
        x0 = 0.d0
        maxiter = 999999
    
        do iter = 1, maxiter
            !x = matmul(T, x0) + c
            if (norma2((x-x0), n)/norma2(x, n) <= tol) then
                Xfinal = x
                stop
            else 
                x0 = x
            end if
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