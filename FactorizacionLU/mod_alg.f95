module algebra

contains

subroutine factorizacion (A,L,U,n)


    
real(8),intent(in)::A(n,n)
integer,intent(in)::n
real(8),intent(out),allocatable:: L(:,:),U(:,:)
integer::i,j,k
real(8) :: sum

allocate (L(n,n))
allocate (U(n,n))

L=0.d0
U=0.d0

L(1,1) = A(1,1)
U(1,1) = 1
 

do k=1,n-1                                       !Es importante el orden de los bucles 
   
    U(1,k+1) = A(1,k+1)/L(1,1)
    
    do i=2,k      
        do j=1,i-1
             sum = L(i,j)*U(j,k+1)
         enddo 
             U(i,k+1) = (A(i,k+1)-sum)/L(i,i)
         enddo  
    
            L(k+1,1) = A(k+1,1)

    do i=2,k      
        do j=1,i-1
             sum = U(j,i)*L(k+1,j)
         enddo 
             L(k+1,i) = (A(k+1,i)-sum)
    enddo

    U(k+1,k+1) = 1                              
   
    do i=1,k
       sum = L(k+1,i)*U(i,k+1)     
    enddo
   
    L(k+1,k+1) = A(k+1,k+1)-sum   
enddo                                           


    endsubroutine


    endmodule
