program pruebas
use subroutines
implicit none

integer :: INFO, i, j, IPIV(2,2)
real*8 :: A(2,2), B(2)

call ascii_lapack

IPIV = 0

write(*,*) "Ejemplo de resolución de sistema de dos variables con dos incógnitas"
write(*,*) "Matricialmente AX=B"
write(*,*) "Introduzca los valores de las filas y columnas de A y B"

! EN EL FUTURO SE EXPANDIRÁ A TAMAXOS NxN
do i = 1, 2
    do j = 1, 2
        write(*,*) "A, fila", i, "columna", j
        read(*,*) A(i,j)
    end do
end do

do i = 1, 2
    write(*,*) "B, componente", i
    read(*,*) B(i)
end do

write(*,*)
write(*,*) "Tu sistema es:"
write(*,*) A(1,:), "|", B(1)
write(*,*) A(2,:), "|", B(2)


call dgesv(2, 1, A, 2, IPIV, B, 2, INFO)
if(info==0) then
    write(*,*)
    write(*,*) B
end if

end program pruebas