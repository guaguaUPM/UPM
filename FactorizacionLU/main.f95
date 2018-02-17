program mates
use ascii_art
use factorizacion_lu
implicit none

integer::n
real(8),allocatable:: A(:,:), LU(:,:), B(:), X(:)
integer::i,j 

call ascii

write(*,*) "Introduce el tamaño de la matriz:"
read(*,*) n

allocate(A(n,n))
allocate(LU(n,n))
allocate(X(n))
allocate(B(n))

! --------- SE PIDEN DATOS AL USUARIO
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

write(*,*) "Su sistema es:"
do i=1, N
    write(*,*) A(i,:), "|", B(i)
end do
! -----------------------------


call factorizar(A, LU, N)

! Debug que muestra L y U
write(*,*)
write(*,*) "Descomposición LU:"
do i=1, N
    write(*,*) LU(i,:)
end do

write(*,*)
write(*,*) "Resolución:"
call resolver(B, LU, X, N)
write(*,*) X


end program mates