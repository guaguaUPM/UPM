program mates
use ascii_art
use algebra
use array_randomizer

implicit none

integer::n
real(8), allocatable    :: A(:,:), A_pivotada(:,:), B(:), B_pivotada(:), X(:),Y(:), X_pivotada(:), L(:,:), U(:,:), Ui(:,:), Li(:,:)
integer, allocatable   :: PERMUTACION(:,:)
real                   :: cpu_start,cpu_finish
integer                :: i, j, modo

call ascii

write(*,*) "Introduce el tamaño de la matriz:"
read(*,*) n

! mold es para no tener que poner (n,n) en cada una, queda mas elegante
allocate(A(n,n))
allocate(A_pivotada, mold=A)
allocate(L, mold=A)
allocate(U, mold=A)
allocate(Li, mold=A)
allocate(Ui, mold=A)
allocate(PERMUTACION(n,n))

allocate(B(n))
allocate(B_pivotada, mold=B)
allocate(X, mold=B)
allocate(Y, mold=B)
allocate(X_pivotada, mold=B)

write(*,*) "Elija el modo: 0=Introducir datos, POR DEFECTO=Asignación aleatoria"
read(*,*) modo
if(modo == 0) then
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
else
    !call randomizar_matriz(A, N)
    !call randomizar_vector(B, N)
    do i = 1, N
        do j = 1, N
            B(i) = (i**3) + 4*i
            A(i,j) = (i**2) + 2*i + (j**3)
        end do
    end do
end if
! -----------------------------

!write(*,*) "Su sistema es:"
!do i=1, N
!    write(*,*) A(i,:), "|", B(i)
!end do

call cpu_time(cpu_start) ! Se empiza a contar el tiempo

! Subrutina de pivotación, devuelve la matriz pivotada con la matriz pivotacion
! FALTA POR HACER: si el 0 está en la útima posición
call pivotar(A, A_pivotada, B, B_pivotada, PERMUTACION, N)
!write(*,*)
!write(*,*) "Matriz de pivotación y sistema pivotado:"
!do i=1, N
!    write(*,*) PERMUTACION(i,:), "||", A_pivotada(i,:), "|", B_pivotada(i)
!end do

! Descomposicion por el método Lucas
call factorizacionLU(A_pivotada, L, U, N)
!write(*,*)
!write(*,*) "Descomposición LU, método LUCAS:"
!do i=1, N
!    write(*,*) L(i,:), "|", U(i,:)
!end do

! Resolucion 
!write(*,*)
!write(*,*) "Resolución:"

call invertir(L,n,Li)
call invertir(U,n,Ui)

Y = MATMUL(Li,b)
X_pivotada =  Matmul(Ui,Y)

!write(*,*) X_pivotada

! Se despivota el vector resolución
X = matmul(X_pivotada, PERMUTACION)

call cpu_time(cpu_finish) ! Se para de contar el tiempo

!! DEBUG, habrá que quitarlo
!write(*,*) "X pivotada vale", X_pivotada
!write(*,*)
!! ----------
!write(*,*) X
write(*,*)
write(*,*) "Se tardó en segundos:", cpu_finish-cpu_start

end program mates