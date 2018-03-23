! CONSISTE EN RESOLVER UNA ECUACIÓN DIFERENCIAL DEL TIPO U¨¨(X) = 0 A TRAVES DE UN PROBLEMA DE CONTORNO
! SE EMPLEA LA MATRIZ DE CONTORNO AL CUADRADO (DERIVADA SEGUNDA) Y SE RESUELVE EL SISTEMA POR EL METODO
! MAS EFICAZ
program calor
implicit none

real*8, allocatable :: matriz(:,:)
real*8 :: x1, x2
integer, parameter :: n=100

!DEBUG
x1=100.d0
x2=200.d0
allocate(matriz(n,n))
call matrizcontorno(matriz, n, x1, x2)
call write_A(matriz, n)

end program calor