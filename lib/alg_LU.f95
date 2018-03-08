subroutine LU_factorizar (A, L, U, n)
    
    implicit none
    real(8),intent(in)::A(n,n)
    integer,intent(in)::n
    real(8),intent(out) ::L(N,N),U(N,N)
    integer::i,j,k
    real(8) :: sum
    
    !Aplicamos el algoritmo matemático asociado a la factorización LU
    
    L=0.d0
    U=0.d0
    
    L(1,1) = A(1,1)
    U(1,1) = 1
     
    do k=1,n-1
                                                   !Es importante el orden de los bucles 
        U(1,k+1) = A(1,k+1)/L(1,1)
              
        do i=2,k      
            do j=1,i-1
                sum = L(i,j)*U(j,k+1)
            end do 
            U(i,k+1) = (A(i,k+1)-sum)/L(i,i)
        end do  
        
        L(k+1,1) = A(k+1,1)
    
        do i=2,k      
            do j=1,i-1
                sum = U(j,i)*L(k+1,j)
            end do 
            L(k+1,i) = (A(k+1,i)-sum)
        end do
    
        U(k+1,k+1) = 1                              
       
        do i=1,k
            sum = L(k+1,i)*U(i,k+1)     
        end do
       
        L(k+1,k+1) = A(k+1,k+1)-sum   
    end do
    
end subroutine LU_factorizar

subroutine resolver_LU (A,B,X,N)
    implicit none
    integer, intent(in) :: N
    real*8, intent(in) :: A(N,N), B(N)
    real*8, intent(out) :: X(N)
    real*8 :: L(N,N), U(N,N), Y(N)

    call LU_factorizar(A,L,U,N)
    call gauss_sustituir(L,B,Y,N, .FALSE.)
    call gauss_sustituir(U,Y,X,N, .TRUE.)
end subroutine resolver_LU