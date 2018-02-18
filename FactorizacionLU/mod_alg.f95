module algebra
contains

subroutine factorizacion (A,L,U,n)
    
    real(8),intent(in)::A(n,n)
    integer,intent(in)::n
    real(8),intent(out),allocatable:: L(:,:),U(:,:)
    integer::i,j,k
    real(8) :: sum

    allocate(L(n,n))
    allocate(U(n,n))

    L=0.d0
    U=0.d0

    L(1,1) = A(1,1)
    U(1,1) = 1
 

    do k=1,n-1                                       !Es importante el orden de los bucles 
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
end subroutine

    subroutine invert(U,m,Ui)
        integer,intent(in)::m 
        integer::i,j
        real*8::k
        real*8,intent(in)::U(m,m)
        real*8,allocatable::B(:,:)
        real*8,intent(inout)::Ui(m,m)
        real*8::d
        allocate(B(m,2*m))
       
        d=0
        Ui=0
        B=0
        
        do i=1,m
            do j=1,m
                d=0
                d=U(i,j)
                B(i,j)=d
            end do 
        end do 
    
    d=0
        do i=1,m
            do j=m+1,2*m
                if(i+m==j)then 
                    B(i,j)=1
                end if 
            end do  
        end do 
        
        !Triangulación Inferior
        
    do i=1,m-1
        !if(abs(B(i,i))<epsilon(1.d0))STOP 
        do j=i+1,m
            k=0
            k=B(j,i)/B(i,i)
            B(j,:)=B(j,:)-k*B(i,:)
        end do 
    end do 
        
    k=0
    !Triangular superior
        
    do i=m,2,-1
        do j=i-1,1,-1
            k=0
            k=B(j,i)/B(i,i)
            B(j,:)=B(j,:)-k*B(i,:)
        end do 
    end do
        
        
    !Dividir para tener la identidad a la izquierda
    do i=1,m
    do j=1,m
            if(i==j)then 
                k=0
                k=B(i,j)
                B(i,:)=B(i,:)/k
            end if 
        end do
    end do 
            
    !Lo pasamos a la matriz inversa como una 3x3
    do i=1,m 
        do j=m+1,2*m
        d=0
        d=B(i,j)
        Ui(i,j-m)=d
        end do 
    end do 
                
    end subroutine invert
    

endmodule