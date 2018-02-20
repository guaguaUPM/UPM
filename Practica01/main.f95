program main
use algebra_lineal
implicit none

real*8, allocatable :: A(:,:), b(:)
integer :: i, j
integer, parameter :: N=100

allocate(A(N,N))
allocate(b(N))

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

!A.- Responder a las siguientes cuestiones sobre el sistema lineal 𝐴𝑥 = 𝑏:

!1. Sea la matriz 𝐴′ el resultado de aplicar el método de Gauss al sistema 𝐴𝑥 = 𝑏. 
!Es decir, la matriz 𝐴´ será de la forma:




end program main