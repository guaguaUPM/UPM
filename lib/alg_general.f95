subroutine pivotar (A_ENTRADA, A_SALIDA, B_ENTRADA, B_SALIDA, PIVOTE, TAMANO)
    ! Evita 0 en la diagonal aplicando una pivotacion con operaciones de Gauss, y tambien a un vector que acompañe a la matriz
    ! Para deshacer el pivote: matmul(PIVOTE, MATRIZ_PIVOTADA)
    implicit none

    integer, intent(in)               :: TAMANO
    real*8,  intent(in)               :: a_entrada(TAMANO,TAMANO), b_entrada(TAMANO)
    real*8,  intent(out)              :: a_salida(TAMANO,TAMANO) , b_salida(TAMANO)
    integer, intent(out)              :: pivote(TAMANO,TAMANO)
    
    integer                           :: i, fila_max, j
    real*8                            :: valor_max, b_auxiliar
    real*8, allocatable               :: AUXILIAR(:)
    integer, allocatable              :: pivote_auxiliar(:)
    

    allocate(AUXILIAR(tamano))
    allocate(pivote_auxiliar(tamano))
        
    A_SALIDA = A_ENTRADA
    B_SALIDA = B_ENTRADA
    
    pivote = 0
    do i=1, tamano
        pivote(i,i) = 1
    end do
    
    ! Bucle que recorre la diagonal
    do i=1, tamano
        ! Si se encuetra un 0 en la diagonal
        if(a_salida(i,i) == 0.d0) then
            ! Resetea el valor maximo, se usarán siempre numeros positivos
            valor_max = 0.d0
            ! Se recorren las posiciones por debajo de la diagonal para encontrar el maximo valor
            do j=i+1, tamano
                if( ABS(A_SALIDA(j,i)) >= valor_max ) then
                    fila_max = j
                    valor_max = a_salida(j,i)
                end if
            end do
    
            ! write(*,*) "FILA MAX:", fila_max, valor_max  ! DEBUG
    
            ! write(*,*) "CAMBIO", i, "por", fila_max      ! DEBUG
    
            ! Cambio por rotacion triangular de la matriz
            AUXILIAR(:)          = a_salida(i,:)
            a_salida(i,:)        = a_salida(fila_max, :)
            a_salida(fila_max,:) = AUXILIAR(:)
    
            ! Cambio por rotacion triangular de la matriz permutacion
            pivote_auxiliar(:)   = pivote(i,:)
            pivote(i,:)          = pivote(fila_max,:)
            pivote(fila_max,:)   = pivote_auxiliar(:)
    
            ! Cambio por rotacion triangular del vector B
            b_auxiliar         = b_salida(i)
            b_salida(i)        = b_salida(fila_max)
            b_salida(fila_max) = b_auxiliar
        end if
    end do
end subroutine pivotar

subroutine inversa (matriz, c, n)
    !============================================================
    ! Inverse matrix
    ! Method: Based on Doolittle LU factorization for Ax=b
    ! Alex G. December 2009
    !-----------------------------------------------------------
    ! input ...
    ! matrix(n,n) - array of coefficients for matrix A
    ! n           - dimension
    ! output ...
    ! c(n,n)      - inverse matrix of A
    !===========================================================
    implicit none 
    integer, intent(in)     :: n
    real*8, intent(in)      :: matriz(n,n)
    real*8, intent(out)     :: c(n,n)
    real*8                  :: a(n,n), L(n,n), U(n,n), b(n), d(n), x(n)
    real*8 :: coeff
    integer :: i, j, k
        
    ! step 0: initialization for matrices L and U and b
    ! Fortran 90/95 aloows such operations on matrices
    a=matriz
    L=0.d0
    U=0.d0
    b=0.d0
        
    ! step 1: forward elimination
    do k=1, n-1
        do i=k+1,n
            coeff=a(i,k)/a(k,k)
            L(i,k) = coeff
            do j=k+1,n
                a(i,j) = a(i,j)-coeff*a(k,j)
            end do
        end do
    end do
        
    ! Step 2: prepare L and U matrices 
    ! L matrix is a matrix of the elimination coefficient
    ! + the diagonal elements are 1.0
    do i=1,n
        L(i,i) = 1.0
    end do
    ! U matrix is the upper triangular part of A
    do j=1,n
        do i=1,j
        U(i,j) = a(i,j)
        end do
    end do
        
    ! Step 3: compute columns of the inverse matrix C
    do k=1,n
        b(k)=1.0
        d(1) = b(1)
        ! Step 3a: Solve Ld=b using the forward substitution
        do i=2,n
            d(i)=b(i)
            do j=1,i-1
              d(i) = d(i) - L(i,j)*d(j)
            end do
        end do
        ! Step 3b: Solve Ux=d using the back substitution
        x(n)=d(n)/U(n,n)
        do i = n-1,1,-1
            x(i) = d(i)
            do j=n,i+1,-1
              x(i)=x(i)-U(i,j)*x(j)
            end do
            x(i) = x(i)/u(i,i)
        end do
        ! Step 3c: fill the solutions x(n) into column k of C
        do i=1,n
            c(i,k) = x(i)
        end do
        b(k)=0.0
    end do
end subroutine inversa

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

subroutine convergencia (T, converge, N)
    implicit none
    integer, intent(in) :: N
    real*8, intent(in)  :: T(N,N)
    logical, intent(out):: converge
    
    real*8              :: autovalor, q0(N)

    q0 = 1.0d0
    call auto_potencia_iter (T, autovalor, 60, q0, N)
    
    if(abs(autovalor) < 1.d0) converge = .true.
    
end subroutine convergencia