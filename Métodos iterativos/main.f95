program main
implicit none
use iterativos

integer ::  i, j, N ! i y j son contadores de bucles, INFO es 1 si se resolvió satisfactoriamente
real*8, allocatable :: A(:,:), B(:)

write(*,*) "Por favor, introduzca el valor de N:"

read(*,*) N
allocate(A(N,N))
allocate(B(N))

write(*,*) "Esciba el valor de las matrices, en doble precisión (0.d0)"

do i = 1, N
    do j = 1, N
        write(*,fmt='(a7,1x,i0,1x,a7,1x,i0)') "A, fila", i, "columna", j
        read(*,*) A(i,j)
    end do
end do

do i = 1, N
    write(*,fmt='(a13,1x,i0)') "B, componente", i
    read(*,*) B(i)
end do

write(*,*)
write(*,*) "Tu sistema AX=B es:"
do i=1, N
    write(*,*) A(i,:), "|", B(i)
end do



end program main