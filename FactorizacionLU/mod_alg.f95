module algebra

contains

subroutine factorizacion (A,L,U,n)


    
real,intent(in)::A(n,n)
integer,intent(in)::n
real,intent(out),allocatable:: L(:,:),U(:,:)
integer::i,j,x,y,k
real(8) :: sum

allocate (L(n,n))
allocate (U(n,n))

L=0.d0
U=0.d0

L(1,1) = A(1,1)

do i=1,n 
    U(i,i)=1
    do j=1,n 
        do k=1,n-1
         U(k+1,k+1) = 1
            if(i==1)then
                U(1,k+1) = A(1,k+1)/L(1,1)
            elseif(i/=1 )then
                do i=2,k  
                 do j=1,i-1
                    sum = L(i,j)*U(j,k+1)
                 enddo 
                    U(i,k+1) = (A(i,k+1)-sum)/L(i,i)
                 enddo
            endif


       
        if(j>i)then 
            L(i,j)=0.d0
        elseif(i>j)then
            U(i,j)=0.d0
        endif
    enddo
    U(i,i)=1
enddo
endsubroutine
endmodule
