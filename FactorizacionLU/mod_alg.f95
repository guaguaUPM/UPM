module algebra

contains

subroutine factorizacion (A,L,U,n)


    
real,intent(in)::A(n,n)
integer,intent(in)::n
real,intent(out),allocatable:: L(:,:),U(:,:)
integer::i,j,x,y


allocate (L(n,n))
allocate (U(n,n))

L=0.d0
U=0.d0
do i=1,n 
    U(i,i)=1
    do j=1,n 
        if(i==1)then
            x=i+1
            y=i+2
        elseif(i==2)then
            x=i-1
            y=i+1
        else 
            x=i-2
            y=i-1
        endif
        L(i,j)=(A(i,j)-(L(i,x)*U(x,j))-(L(i,y)*U(y,j)))
        U(i,j)=(A(i,j)-(L(i,x)*U(x,j)))/L(i,i)
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
