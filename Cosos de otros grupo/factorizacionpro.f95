program factorizacionpro

use algebra_lineal 

implicit none

integer :: m,i,j,k,q
real(8), allocatable :: A(:,:)
real(8), allocatable :: AO(:,:)
real(8), allocatable :: P(:,:)
real(8), allocatable :: L(:,:)
real(8), allocatable :: Li(:,:)
real(8), allocatable :: Pi(:,:)
real(8), allocatable :: U(:,:)
real(8), allocatable :: Ui(:,:)
real(8), allocatable :: B(:,:)
real(8), allocatable :: D(:,:)

write(*,*) 'Introducir tamaño matriz'
read(*,*) m 

allocate(A(m,m))
allocate(AO(m,m))
allocate(P(m,m))
allocate(L(m,m))
allocate(Li(m,m))
allocate(Pi(m,m))
allocate(U(m,m))
allocate(Ui(m,m))
allocate(B(m,m))
allocate(D(m,m))
A=0.d0
do i=1,m 
    do j=1,m 
        write(*,*) 'Introducir valor de la posicion', i, j
        read(*,*)  A(i,j)
    enddo
enddo
AO=A
L=0.d0
P=0.d0
Li=0.d0
Pi=0.d0
U=0.d0
B=0.d0
Ui=0.d0

do i=1,m
    write(*,*) A(i,:) 
enddo 
do i=1,m 
    U(i,i)=1
enddo
do k=1,m 
    P(k,k)=1
enddo
do i=1,m-1
    Pi=0
    Li=0
    do k=1,m 
        Pi(k,k)=1
    enddo
    do k=1,m 
        Li(k,k)=1
    enddo
    call pivotar(A, Pi, i)
    write(*,*) 'A'
    do q=1,m 
        write(*,*) A(q,:)
    enddo 
    write(*,*) 'Pi'
    do q =1,m 
        write(*,*) Pi(q,:)
    enddo 
    call eliminacion(A,Li,m,i)
    write(*,*) 'A'
    do q =1,m 
        write(*,*) A(q,:)
    enddo 
    write(*,*) 'Li'
    do q =1,m 
        write(*,*) Li(q,:)
    enddo 
    P=matmul(Pi,P)
    write(*,*) 'P'
    do q =1,m 
        write(*,*) P(q,:)
    enddo 
    B=matmul(Pi,U)
    U=matmul(Li,B)
    write(*,*) 'U'
    do q=1,m 
        write(*,*) U(q,:)
    enddo 
enddo 
call aproximar(P,m)
call aproximar(L,m)
U=matmul(U, AO)
call aproximar(U,m)
call inversa(U,m, Ui)
write(*,*) 'Ui'
do q=1,m 
    write(*,*) Ui(q,:)
enddo
!P=transpose(P)
D=matmul(P,AO)
L=matmul(D,Ui)
call aproximar(L,m)

write(*,*) 'La factorizacion en forma PA=LU de la matriz dada es'
write(*,*) 'P='
do q=1,m 
    write(*,*) P(q,:)
enddo
write(*,*) 'L='
do q=1,m 
    write(*,*) L(q,:)
enddo
write(*,*) 'U='
do q=1,m 
    write(*,*) U(q,:)
enddo
end program factorizacionpro