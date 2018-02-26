program main
use alg_general
use alg_LU
implicit none

integer :: N, i, j, ENTRADA_I, ENTRADA_J
real*8, allocatable :: A(:,:), B(:), DUMMY_A(:,:), DUMMY_B(:), X(:), L(:,:), U(:,:), L_normalizada(:,:)


! Se asigna el tamaño a las matrices
write(*,*) "Introduzca el tamaño N de la Matriz:"
read(*,*) N
allocate(A(N,N))
allocate(DUMMY_A, mold=A)
allocate(L, mold=A)
allocate(L_normalizada, mold=A)
allocate(U, mold=A)
allocate(B(N))
allocate(DUMMY_B, mold=B)
allocate(X, mold=B)


! Se define A (Segun enunciado)
do i=1, N
    do j=1, N
        if(i==j) then
            A(i,j) = 100.d0*(i+j)
        else
            A(i,j) = 1.d0*(i-j)
        end if
    end do
end do
! Se define B
do i=1, N
    B(i) = i/10.d0
end do
! Y se le muestra al usuario
if (N<=6) then
    write(*,*) "Su sistema es:"
    do i=1, N
        write(*,*) A(i,:), "|", B(i)
    end do
end if

!-----------------------------
! PRIMER EJERCICIO
! No se aplica el pivote porque no habrá ceros en la diagonal
! SI CAMBIA LA DEFINCION DE A Y B, APLICAR PIVOTE
write(*,*)
write(*,*) "EJERCICIO 1:"
write(*,*) "Aplicación de GAUSS:"

call gauss_triangular(A, DUMMY_A, B, DUMMY_B, N)

if (N>6) then
    write(*,*) "¿Qué elemento desea? (i,j)"
    read(*,*) ENTRADA_I
    read(*,*) ENTRADA_J
    write(*,*) DUMMY_A(ENTRADA_I, ENTRADA_J)
else
    do i=1, N
        write(*,*) DUMMY_A(i,:)
    end do
end if

!--------------------
! SEGUNDO EJERCICIO
write(*,*)
write(*,*) "EJERCICIO 2:"
write(*,*) "Descomposición en LU:"

call LU_factorizar(A, L, U, N)
if (N>6) then
    write(*,*) "¿Qué elemento de L y U desea? (i,j)"
    read(*,*) ENTRADA_I
    read(*,*) ENTRADA_J
    write(*,*) L(ENTRADA_I, ENTRADA_J), "/", U(ENTRADA_I, ENTRADA_J)
else
    do i=1, N
        write(*,*) L(i,:), "|", U(i,:)
    end do
end if

!--------------------
! TERCER EJERCICIO
write(*,*)
write(*,*) "EJERCICIO 3:"
write(*,*) "GAUSS"
call gauss_sustituir(DUMMY_A, DUMMY_B, X, N, .true.)
write(*,*) X
read(*,*)

write(*,*) "LU"
! A=LU, Ly=B > Ux=y
L_normalizada = L
do i = 1, N
    B(i) = B(i) / L_normalizada(i,i)
    L_normalizada(i,:) = L_normalizada(i,:) / L_normalizada(i,i)
end do
call gauss_sustituir(L_normalizada, B, DUMMY_B, N, .false.)
call gauss_sustituir(U, DUMMY_B, X, N, .true.)
write(*,*) X


end program main