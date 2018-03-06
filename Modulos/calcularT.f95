module matriz

contains
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

end module