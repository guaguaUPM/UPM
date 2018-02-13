program mates
use algebra
use ascii_art
implicit none



integer::n
real,allocatable::A(:,:),L(:,:),U(:,:)
integer::i,j 

write(*,*) "Introduce el tamaño de la matriz:"
read(*,*) n

allocate (A(n,n))
call ascii
do i=1,n
    do j=1,n
        write(*,*) 'escriba un numero para',i,j
        read(*,*) A(i,j)
    enddo
enddo

call factorizacion (A,L,U,n)

do i=1,n
    write(*,*) L(i,:)
enddo

do i=1,n
    write(*,*) U(i,:)
enddo

end program mates