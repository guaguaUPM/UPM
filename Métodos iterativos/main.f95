program main
implicit none
use iterativos

integer ::  i, j, N ! i y j son contadores de bucles, INFO es 1 si se resolvió satisfactoriamente
real*8, allocatable :: A(:,:), B(1,:), X(:)

write(*,*) "Por favor, introduzca el valor de N:"

read(*,*) N
allocate(A(N,N))
allocate(B(1,N))
allocate(X(N))

write(*,*) "Esciba el valor de las matrices, en doble precisión (0.d0)"

do i = 1, N
    do j = 1, N
        write(*,fmt='(a7,1x,i0,1x,a7,1x,i0)') "A, fila", i, "columna", j
        read(*,*) A(i,j)
    end do
end do

do i = 1, N
    write(*,fmt='(a13,1x,i0)') "B, componente", i
    read(*,*) B(1,i)
end do

write(*,*)
write(*,*) "Tu sistema AX=B es:"
do i=1, N
    write(*,*) A(i,:), "|", B(1,i)
end do

call jacobi (A, X, b, 0.00001d0)

write (*,*) X

end program main