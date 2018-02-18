program mates
use ascii_art
use factorizacion_lu
use algebra
implicit none

integer::n
real(8),allocatable    :: A(:,:), A_pivotada(:,:), LU(:,:), B(:), B_pivotada(:), X(:), X_pivotada(:), L(:,:), U(:,:)
integer, allocatable   :: PERMUTACION(:,:)
real                   :: cpu_start,cpu_finish
integer                ::i,j 

call ascii

write(*,*) "Introduce el tamaño de la matriz:"
read(*,*) n

! mold es para no tener que poner (n,n) en cada una, queda mas elegante
allocate(A(n,n))
allocate(LU, mold=A)
allocate(A_pivotada, mold=A)
allocate(L, mold=A)
allocate(U, mold=A)
allocate(PERMUTACION(n,n))

allocate(B(n))
allocate(B_pivotada, mold=B)
allocate(X, mold=B)
allocate(X_pivotada, mold=B)


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




call cpu_time(cpu_start) ! Se empiza a contar el tiempo

! Subrutina de pivotación, devuelve la matriz pivotada con la matriz pivotacion
! FALTA POR HACER: si el 0 está en la útima posición
call pivotar(A, A_pivotada, B, B_pivotada, PERMUTACION, N)
write(*,*)
write(*,*) "Matriz de pivotación y sistema pivotado:"
do i=1, N
    write(*,*) PERMUTACION(i,:), "||", A_pivotada(i,:), "|", B_pivotada(i)
end do

! Descomposicion por el método Fernando
! A BORRAR, el metodo Lucas funciona mejor
call factorizar(A_pivotada, LU, N)
write(*,*)
write(*,*) "Descomposición LU, método Fernando"
do i=1, N
    write(*,*) LU(i,:)
end do

! Descomposicion por el método Lucas
call factorizacion(A_pivotada, L, U, N)
write(*,*)
write(*,*) "Descomposición LU, método LUCAS:"
do i=1, N
    write(*,*) L(i,:), "|", U(i,:)
end do


! Resolucion por metodo Fernando
! A BORRAR Y SUSTITUIR POR METODO FERNANDO
write(*,*)
write(*,*) "Resolución:"
call resolver(B_pivotada, LU, X_pivotada, N)

! Se despivota el vector resolución
X = matmul(X_pivotada, PERMUTACION)

call cpu_time(cpu_finish)

write(*,*) "X pivot vale", X_pivotada
write(*,*)
write(*,*) X, "Se tardó en segundos:", cpu_finish-cpu_start


end program mates