program main
use algebra_lineal
implicit none

real*8, allocatable :: A(:,:), At(:,:), b(:)
integer :: i, j

integer, parameter :: N=100 !DEPENDE DEL EXAMEN
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

!A.- Responder a las siguientes cuestiones sobre el sistema lineal AX = B:

    !1. Sea la matriz 𝐴t el resultado de aplicar el método de Gauss al sistema AX = B. 

    call triangular_superior_gauss (A, At)
    write(*,*) "Su matriz At resultado es:" 
    do i = 1, N
        write(*,*) A(i,:)
    end do

    !2. Sean las matrices L y U el resultado de hacer una descomposición de tipo A=LU 
    !(descomposición LU sin permutación).

end program main