program main
use algebra_lineal
implicit none

real*8, allocatable :: A(:,:), b(:), X(:), b_2(:)
integer :: i, j
real*8 :: tol
integer, parameter :: N=10 !DEPENDE DEL EXAMEN
allocate(A(N,N))
allocate(b(N))
allocate(X(N))
allocate(B_2(N))

!Sean la matriz A de tamaño 100 x 100 y el vector b de tamaño 100 definidos como:
do i=1, N
    do j=1, N
        if(i==j) then
            A(i,j) = 10.d0*i
        else
            A(i,j) = 1.d0
        end if
    end do
end do
! Se define B
do i = 1, N
    B_2(i) = i*1.d0
end do
B = matmul(A,B_2)

!A.- Responder a las siguientes cuestiones sobre el sistema lineal AX = B:
    write(*,*) "Escriba un valor para la tolerancia de gauss-seidel y jacobi"
    read(*,*) tol

    call jacobi (A, X, b, tol)
    write(*,*) "Xi de Jacobi", X
    read(*,*)

    call gauss_seidel (A, X, b, tol)
    write(*,*) "Xi de Gauss-Seidel", X
    read(*,*)

end program main