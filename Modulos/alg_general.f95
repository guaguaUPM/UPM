module alg_general
contains

subroutine pivotar (ENTRADA, SALIDA, B_ENTRADA, B_SALIDA, PERMUTACION, TAMANO)
    ! Evita 0 en la diagonal aplicando una pivotacion con operaciones de Gauss, y tambien a un vector que acompañe a la matriz
    ! Para deshacer el pivote: matmul(PERMUTACION, MATRIZ_PIVOTADA)
    implicit none
    
    real*8, intent(in)                :: entrada(:,:), b_entrada(:)
    real*8,intent(out)                :: salida(:,:) , b_salida(:)
    integer,              intent(out) :: permutacion(:,:)
    integer, intent(in)               :: tamano
    
    integer                           :: i, fila_max, j
    real*8                            :: valor_max, b_auxiliar
    real*8, allocatable               :: AUXILIAR(:)
    integer, allocatable              :: permutacion_auxiliar(:)
    

    allocate(AUXILIAR(tamano))
    allocate(permutacion_auxiliar(tamano))
        
    SALIDA=ENTRADA
    B_SALIDA = B_ENTRADA
    
    permutacion = 0
    do i=1, tamano
        permutacion(i,i) = 1
    end do
    
    ! Bucle que recorre la diagonal
    do i=1, tamano
        ! Si se encuetra un 0 en la diagonal
        if(salida(i,i) == 0.d0) then
            ! Resetea el valor maximo, se usarán siempre numeros positivos
            valor_max = 0.d0
            ! Se recorren las posiciones por debajo de la diagonal para encontrar el maximo valor
            do j=i+1, tamano
                if( ABS(SALIDA(j,i)) >= valor_max ) then
                    fila_max = j
                    valor_max = salida(j,i)
                end if
            end do
    
            ! write(*,*) "FILA MAX:", fila_max, valor_max  ! DEBUG
    
            ! write(*,*) "CAMBIO", i, "por", fila_max      ! DEBUG
    
            ! Cambio por rotacion triangular de la matriz
            AUXILIAR(:)        = salida(i,:)
            salida(i,:)        = salida(fila_max, :)
            salida(fila_max,:) = AUXILIAR(:)
    
            ! Cambio por rotacion triangular de la matriz permutacion
            permutacion_auxiliar(:)   = PERMUTACION(i,:)
            PERMUTACION(i,:)          = PERMUTACION(fila_max,:)
            PERMUTACION(fila_max,:)   = permutacion_auxiliar(:)
    
            ! Cambio por rotacion triangular del vector B
            b_auxiliar         = b_salida(i)
            b_salida(i)        = b_salida(fila_max)
            b_salida(fila_max) = b_auxiliar
    
            ! write(*,*) "AUXILIAR vale", AUXILIAR   ! DEBUG
        end if
    end do
end subroutine pivotar

function inversa (U)
    real*8, intent(in)             :: U(:,:)

    real*8, allocatable            :: inversa(:,:)
    real*8, allocatable            :: B(:,:)
    real*8                         :: d, k
    integer                        :: i, j, n

    n = size(U,1)
    allocate(inversa(n,n))
    allocate(B(n,2*n))
       
    d=0
    inversa=0
    B=0
            
    do i=1, n
        do j=1, n
            d=0
            d=U(i,j)
            B(i,j)=d
        end do 
    end do 
        
    d=0
    do i=1,n
        do j=n+1,2*n
            if (i+n==j) then 
                B(i,j)=1
            end if 
        end do  
    end do 
           
    !Triangulación Inferior  
    do i=1, n-1
        !if(abs(B(i,i))<epsilon(1.d0)) STOP 
        do j=i+1, n
            k=0
            k=B(j,i)/B(i,i)
            B(j,:)=B(j,:)-k*B(i,:)
        end do 
    end do 
        
    k=0

    !Triangular superior   
    do i=n, 2, -1
        do j=i-1, 1, -1
            k=0
            k=B(j,i)/B(i,i)
            B(j,:)=B(j,:)-k*B(i,:)
        end do 
    end do
            
            
        !Dividir para tener la identidad a la izquierda
    do i=1, n
        do j=1, n
                if (i==j) then 
                k=0
                k=B(i,j)
                B(i,:)=B(i,:)/k
            end if 
        end do
    end do 
            
    !Lo pasamos a la matriz inversa como una 3x3
    do i=1, n
        do j=n+1, 2*n
        d=0
        d=B(i,j)
        inversa(i,j-n)=d
        end do 
    end do 
                    
end function

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

subroutine matrizT (A)
    ! Argumentos de la subrutina
    real(8), intent(in) :: A(:,:)          !
            
    ! Variables locales
    integer :: n                     ! Dimensión del problema A(n,n) b(n) X(n)
    real(8), allocatable :: L(:,:)
    real(8), allocatable :: D(:,:)
    real(8), allocatable :: U(:,:)
    real(8), allocatable :: T(:,:)
    integer :: j, k
            
    n = size(A,1)
    allocate(L(n,n))
    allocate(D(n,n))
    allocate(U(n,n))
    allocate(T(n,n))
            
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
    T = matmul((-1)*inversa(D),(U+L))
    
end subroutine matrizT

end module alg_general