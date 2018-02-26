program main
use algebra_lineal
implicit none

real*8, allocatable :: A(:,:), At(:,:), L(:,:), U(:,:), b(:), X(:)
integer :: i, j
real*8 :: tol
integer, parameter :: N=3!DEPENDE DEL EXAMEN
allocate(A(N,N))
allocate(At(N,N))
allocate(L(N,N))
allocate(U(N,N))
allocate(b(N))
allocate(X(N))

do i=1,n
    do j=1,n
        write(*,*)"Escriba la posicion",i,j
        read(*,*) A(i,j)
    enddo
enddo

    write(*,*) inversa(A)
end program main