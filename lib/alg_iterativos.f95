
subroutine convergencia (T, converge, N)
    implicit none
    integer, intent(in) :: N
    real*8, intent(in)  :: T(N,N)
    logical, intent(out):: converge

    real*8              :: autovalor

    call auto_potencia_iter (T, autovalor, 60, 1.d0, N)

    if(abs(autovalor) < 1.d0) converge = .true.

end subroutine convergencia


!ITERATIVOS REQUIEREN QUE EL SISTEMA CONVERJA
!REQUIERE QUE NO EXISTAN CEROS EN LA DIAGONAL (PREVIO PIVOTAMIENTO) 

subroutine resolver_jacobi_tol (A, Xfinal, b, tol, N)

    ! Argumentos de la subrutina
    real(8), intent(in) :: A(N,N)          !
    real(8), intent(in) :: b(N)            !
    real(8), intent(out):: Xfinal(N)       !
    real(8), intent(in) :: tol             !
    integer, intent(in) :: N               ! Dimensión del problema A(n,n) b(n) X(n)

    ! Variables locales
    real(8), allocatable :: x0(:)
    real(8), allocatable :: x(:)
    integer :: iter, i,j, maxiter
    real(8) :: sum1, normaX_X0, normaX
    
    allocate(x0(N))
    allocate(x(N))
    
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
        
        call norma2(normaX_X0,X-x0,n)
        call norma2(normaX,x,n)

        if (normaX_X0/normaX<= tol) then
            Xfinal = x
            return
        else 
            x0 = x
        end if
    enddo    
end subroutine resolver_jacobi_tol

subroutine resolver_jacobi_iter (A, Xfinal, b, Maxiter, N)

        ! Argumentos de la subrutina
        real(8), intent(in) :: A(N,N)          !
        real(8), intent(in) :: b(N)            !
        real(8), intent(out):: Xfinal(N)       !
        integer, intent(in) :: Maxiter         !
        integer, intent(in) :: N               ! Dimensión del problema A(n,n) b(n) X(n)
        
    
        ! Variables locales
        real(8), allocatable :: x0(:)
        real(8), allocatable :: x(:)
        integer :: iter, i,j
        real(8) :: sum1

        allocate(x0(N))
        allocate(x(N))
        
        !Primera semilla (primer valor establecido de x para iniciar la iteración)
        x0 = 0.d0
        x  = 0.0d0
        
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
            
                x0 = x
           
        enddo   
            
        Xfinal = x  
end subroutine resolver_jacobi_iter    

subroutine resolver_gauss_seidel_tol (A, Xfinal, b, tol, N)
    implicit none

    ! Argumentos de la subrutina
    integer, intent(in) :: n               ! Dimensión del problema A(n,n) b(n) X(n)
    real(8), intent(in) :: A(n,n)          !
    real(8), intent(in) :: b(n)            !
    real(8), intent(out) :: Xfinal(n)      !
    real(8), intent(in) :: tol             !
        
    ! Variables locales
    real(8), allocatable :: x0(:)
    real(8), allocatable :: x(:)
    real(8) :: sum1, sum2, normaX_X0, normaX
    integer :: iter, i, j, maxiter
    
    allocate(x0(n))
    allocate(x(n))
                
    !Primera semilla (primer valor establecido de x para iniciar la iteración)
    x0 = 0.0d0
    x  = 0.0d0
    maxiter = 9999999 !Numero de iteraciones
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
            call norma2(normaX_X0,X-x0,n)
            call norma2(normaX,x,n)
    
            if (normaX_X0/normaX<= tol) then
                Xfinal = x
                return
            else 
                x0 = x
            end if    
    enddo      
end subroutine resolver_gauss_seidel_tol

subroutine resolver_gauss_seidel_iter (A, Xfinal, b, maxiter, N)
        implicit none

        ! Argumentos de la subrutina
        integer, intent(in) :: n               ! Dimensión del problema A(n,n) b(n) X(n)
        real(8), intent(in) :: A(n,n)          !
        real(8), intent(in) :: b(n)            ! 
        real(8), intent(out) :: Xfinal(n)      !
        integer, intent(in) :: maxiter
        ! Variables locales
    
        real(8), allocatable :: x0(:)
        real(8), allocatable :: x(:)
        real(8) :: sum1, sum2
        integer :: iter, i, j
        
        allocate(x0(n))
        allocate(x(n))
                    
        !Primera semilla (primer valor establecido de x para iniciar la iteración)
        x0 = 0.0d0
        x  = 0.0d0
        
         !Numero de iteraciones
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
            
                x0 = x
                
        enddo   
        Xfinal = x   
end subroutine resolver_gauss_seidel_iter