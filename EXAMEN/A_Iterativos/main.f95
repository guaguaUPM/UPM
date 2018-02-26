program main
use algebra_lineal
implicit none

real*8, allocatable :: A(:,:), b(:), X(:)
integer :: i, j
real*8 :: tol
integer, parameter :: N=100 !DEPENDE DEL EXAMEN
allocate(A(N,N))
allocate(b(N))
allocate(X(N))

!Sean la matriz A de tamaño 100 x 100 y el vector b de tamaño 100 definidos como:
do i = 1, N
    do j = 1, N
        b(i) = (i*(1.d0))/10
        if (i==j) then
            A(i,j) = 100*(i+j)
        else
            A(i,j) = i-j
        end if
    end do
end do

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