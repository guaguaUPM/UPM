program mates
use ascii_art
use factorizacion_lu
implicit none



integer::n
real(8),allocatable:: A(:,:),L(:,:),U(:,:)
integer::i,j 

call ascii
write(*,*) "Introduce el tamaño de la matriz:"
read(*,*) n

allocate (A(n,n))
do i = 1, N
    do j = 1, N
        write(*,fmt='(a7,1x,i0,1x,a7,1x,i0)') "A, fila", i, "columna", j
        read(*,*) A(i,j)
    end do
end do

call factorizar(A, L, U, N)
                                      !Debug que muestra L y U
do i=1, N
    write(*,*) L(i,:), "|", U(i, :)
end do

                                      !Solucion


end program mates