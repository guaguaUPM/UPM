module alg_iterativos 
implicit none

contains

    !ITERATIVOS REQUIEREN QUE EL SISTEMA CONVERJA
    !REQUIERE QUE NO EXISTAN CEROS EN LA DIAGONAL (PREVIO PIVOTAMIENTO) 

    subroutine jacobi (A, Xfinal, b, tol)

        ! Argumentos de la subrutina
        real(8), intent(in) :: A(:,:)          !
        real(8), intent(in) :: b(:)            ! DIMENSIONES ASUMIDAS
        real(8), intent(out) :: Xfinal(:)    !
        real(8), intent(in) :: tol             !
    
        ! Variables locales
        integer :: n                     ! Dimensión del problema A(n,n) b(n) X(n)
        real(8), allocatable :: x0(:)
        real(8), allocatable :: x(:)
        integer :: iter, i,j, maxiter
        real(8) :: sum1
        n = size(A,1)
        allocate(x0(n))
        allocate(x(n))
    
        !Primera semilla (primer valor establecido de x para iniciar la iteración)
        x0 = 0.d0
        x  = 0.0d0
        maxiter = 999999
    
        do iter = 1, maxiter
            do i=1,n
                sum1=0  
                do j=1,n
                    if(j/=i) then
                        sum1 = sum1 + A(i,j)*x0(j)
                    endif    
                enddo
                x(i) = (1/A(i,i)) * (B(i) - sum1)    
            enddo
            if (norma2((x-x0), n)/norma2(x, n) <= tol) then
                Xfinal = x
                return
            else 
                x0 = x
            end if
        end do
    
    end subroutine
    
    subroutine gauss_seidel (A, Xfinal, b, tol)
    
        ! Argumentos de la subrutina
        real(8), intent(in) :: A(:,:)          !
        real(8), intent(in) :: b(:)            ! DIMENSIONES ASUMIDAS
        real(8), intent(out) :: Xfinal(:)    !
        real(8), intent(in) :: tol             !
        
        ! Variables locales
        integer :: n                     ! Dimensión del problema A(n,n) b(n) X(n)
        real(8), allocatable :: x0(:)
        real(8), allocatable :: x(:)
        real(8) :: sum1, sum2
        integer :: iter, i, j, maxiter
        
        n = size(A,1)
        allocate(x0(n))
        allocate(x(n))
                
        !Primera semilla (primer valor establecido de x para iniciar la iteración)
        x0 = 0.0d0
        x  = 0.0d0
        maxiter = 2 !Numero de iteraciones
        do iter = 1, maxiter !Iteraciones
            do i=1,n
                sum1=0  
                sum2=0            
                do j=1,i-1
                    sum1 = sum1 + A(i,j) * x(j)
                enddo
                do j=i+1,n
                    sum2 = sum2 + A(i,j) * x0(j)
                enddo                           
                x(i) = (1/A(i,i)) * (B(i) - sum1 - sum2)           
            enddo        
            if (norma2((x-x0), n)/norma2(x, n) <= tol) then
                Xfinal = x
                return
            else 
                x0 = x
            endif     
        enddo      
    end subroutine
    
    function norma2 (vector, n)
        real(8), intent(in) :: vector(:)
        integer, intent(in) :: n
        real(8) :: norma2
        integer :: i
    
        norma2 = 0.d0
        do i = 1, n
            norma2 = norma2+vector(i)**2
        enddo
        norma2 = sqrt(norma2) 
        
    end function
        
end module