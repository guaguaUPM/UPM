program main
use algebra_lineal
implicit none

real*8, allocatable :: A(:,:), At(:,:), L(:,:), U(:,:), b(:), X(:)
integer :: i, j

integer, parameter :: N=2 !DEPENDE DEL EXAMEN
allocate(A(N,N))
allocate(At(N,N))
allocate(L(N,N))
allocate(U(N,N))
allocate(b(N))
allocate(X(N))



do i=1,n
    do j=1,n
        read(*,*) A(i,j)
    enddo
    enddo
do i=1,n

   read(*,*) b(i)
enddo   

   call gauss_seidel (A, X, b, 0.d0)

write (*,*) X


end program main