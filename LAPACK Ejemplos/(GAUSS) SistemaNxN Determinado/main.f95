program pruebas
use ascii_art
use array_randomizer
implicit none

integer              :: INFO, i, j, IPIV(2,2), N, modo ! i y j son contadores de bucles, INFO es 1 si se resolvió satisfactoriamente
real                 :: cpu_start,cpu_finish
real*8, allocatable  :: A(:,:), B(:)

call ascii_lapack

IPIV = 0 

write(*,*) "Ejemplo del uso de la función DGESV para la resolución de sistemas compatibles AX=B de N ecuaciones con N incógnitas"
write(*,*) "Por favor, introduzca el valor de N:"

read(*,*) N
allocate(A(N,N))
allocate(B(N))


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
    call randomizar_matriz(A, N)
    call randomizar_vector(B, N)
end if
! -----------------------------
    
write(*,*) "Su sistema es:"
do i=1, N
    write(*,*) A(i,:), "|", B(i)
end do



call cpu_time(cpu_start)
call dgesv(2, 1, A, 2, IPIV, B, 2, INFO)
call cpu_time(cpu_finish)

if(info==0) then
    write(*,*)
    write(*,*) B
    write(*,*) "Sistema resuelto satisfactoriamente, se tardó (s):", cpu_finish - cpu_start
else
    write(*,*) "El sistema no es compatible determinado"
end if

deallocate(A)
deallocate(B)

end program pruebas