program mates
use algebra
use ascii_art
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

call factorizacion (A,L,U,n)
                                      !Debug que muestra L y U
do i=1,n
    write(*,*) L(i,:)
enddo

do i=1,n
    write(*,*) U(i,:)
enddo

                                      !Solucion


end program mates