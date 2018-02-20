!DGETC2 computes the LU factorization with complete pivoting of the general n-by-n matrix.
program fact_lu
use ascii_art
implicit none

integer              :: i, j
integer              :: INFO, M, N, LDA             !INFO es 0 si se resolvió satisfactoriamente.
integer, allocatable :: IPIV(:)  !Índices de pivotamiento (fila y columna respectivamente).
real                 :: cpu_start,cpu_finish
real*8, allocatable  :: A(:,:), L(:,:), U(:,:)
    
call ascii_lapack
    
write(*,*) "Ejemplo del uso de la función DGETC2 para la factorización LU de una matriz A(NxN)"
write(*,*) "Por favor, introduzca el valor de N:"
    
read(*,*) N

allocate(A(N,N))
allocate(L(N,N))
allocate(U(N,N))
allocate(IPIV(N))

IPIV = 0
M=N
LDA=N
    
write(*,*) "Esciba el valor de las matrices, en doble precisión (0.d0)"
    
do i = 1, N
    do j = 1, N
        write(*,fmt='(a7,1x,i0,1x,a7,1x,i0)') "A, fila", i, "columna", j
        read(*,*) A(i,j)
    end do
end do
    
write(*,*)
write(*,*) "Su matriz A es:"
do i=1, N
    write(*,*) A(i,:)
end do
    
call cpu_time(cpu_start)
    
call dgetrf(M,N,A,LDA,IPIV,INFO)

call cpu_time(cpu_finish)

do i=1, N
    write(*,*) A(i,:)
end do

if(info==0) then
    write(*,*)
    write(*,*) "Matriz factorizada correctamente, se tardó (s):", cpu_finish - cpu_start
    write(*,*) 
    !do i=1, N
        !write(*,*) L(i,:), "|", U(i,:)
    !end do
end if

end program fact_lu