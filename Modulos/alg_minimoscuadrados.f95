program main
implicit none

! Aproximación de un polinomio a una nube de puntos mediante el método de los mínimos cuadrados
! Uso de librerías LAPACK para la resolución del sistema resultante

! TAMANO es el número de parejas de puntos contenidos en el archivo
! orden_polinomio es introducido por el usuario, el polinomio irá desde x^0 hasta x^orden_polinomio
! i,l,k contadores de bucles
integer              :: TAMANO, orden_polinomio,      i,l,k
! PUNTO contiene los puntos (x,y) del archivo data_file.txt
! argumento es el vector que contiene a los argumentos del polinomio resultado
! B y M proceden del método MA=B para halar el vector A de argumentos
! IPIV_lapack es una matriz dummy para que no pete la función lapack solo esta para rellenar el argumento
real*8, allocatable  :: PUNTO(:,:), argumento(:), B(:), M(:,:), IPIV_lapack(:,:)
real*8               :: error, buffer


! Se abre el archivo
open(unit=10, file='data_file.txt', status='old')

! La primera línea es la cantidad de datos
read(10,*) TAMANO
allocate(PUNTO(TAMANO,2))

! Se procede a leer el archivo de puntos. No hay excepciones con INFO, se presupone que el arhivo se lee correctamente
do i=1, TAMANO
    read(10,*) PUNTO(i,1), PUNTO(i,2)
end do
close(10) ! Se cierra para evitar memory leaks





! Se pide al usuario el orden del polinomio
write(*,*) "Introduzca el orden p del polinomio a construir:"
read(*,*) orden_polinomio
allocate(argumento(0:orden_polinomio))
allocate(B(0:orden_polinomio))
allocate(M(0:orden_polinomio, 0:orden_polinomio))
allocate(IPIV_lapack(orden_polinomio+1, orden_polinomio+1))





! Determinación del vector B. Interpretación literal de la fórmula matemática en formulas.png
B = 0.d0 ! Importante, es un sumatorio que empieza desde 0
do l=0, orden_polinomio
    do i=1, TAMANO
        B(l) = B(l) + ((PUNTO(i,1))**l) * PUNTO(i,2)
    end do
end do

! Determinación de la matriz M. Interpretación literal de la fórmula matemática en formulas.png
M = 0.d0 ! Importrante, sumatorio que empieza desde 0
do l=0, orden_polinomio
    do k=0, orden_polinomio
        do i=1, TAMANO
            M(l,k) = M(l,k) + ((PUNTO(i,1))**l) * ((PUNTO(i,1))**k)
        end do
    end do
end do    

! Se le muestra el sistema al usuario si no es demasiado grande para mostrarlo en pantalla
if(orden_polinomio < 7) then
    write(*,*) "M|B (MA=B):"
    do i = 0, orden_polinomio
        write(*,*) M(i,:), "|", B(i)
    end do
end if





! Se procede a la resolución con DGESV
argumento = B
call dgesv(orden_polinomio+1, 1, M, orden_polinomio+1, IPIV_lapack, argumento, orden_polinomio+1, i)





! Se le muestra al usuario los argumentos del polinomio
if(orden_polinomio < 20) then
    write(*,*) "Argumentos:"
    do i = 0, orden_polinomio
        write(*,*) argumento(i)
    end do
else
    write(*,*) "Tamaño demasiado grande, ¿que argumento necesita?"
    read(*,*) i
    write(*,*) argumento(i)
end if



! Calculo del error con la formula matematica de formulas.png y muestra al usuario
error = 0.d0
do i=1, TAMANO
    buffer = 0.d0
    do l=0, orden_polinomio
        buffer = buffer + argumento(l) * (PUNTO(i,1))**l
    end do
    error = error + (PUNTO(i,2) - buffer)**2
end do
write(*,*) "Error:", error

end program main