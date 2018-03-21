program main
use funciones
implicit none

real*8 :: inicio, fin, area
integer :: particiones

write(*,*) "¿Desde que punto desea la integral?"
read(*,*) inicio
write(*,*) "¿Hasta que punto desea la integral?"
read(*,*) fin
write(*,*) "¿Cuantas particiones desea?"
read(*,*) particiones

call integral_riemann(campana,inicio,fin,particiones,area)
write(*,*) "Integral de reimann:", area

call integral_trapcio(campana,inicio,fin,particiones,area)
write(*,*) "Integral trapecio:  ", area

call integral_simpson(campana,inicio,fin,particiones,area)
write(*,*) "Integral simpson:   ", area

end program main