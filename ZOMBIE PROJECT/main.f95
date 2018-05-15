! Mayo 2018
! Trabajo hecho por el Grupo 3 de Informática del M6.
! Integrantes:
!  Fernando Ayats Llamas
!  Alejandro Cervigni Sebastián
!  Fran
!  Alvaro Gonzalez Villarreal
!  Lucs 

! A partir de un modelo de ecuaciones diferenciales, se simula un apocalipsis zombie y el crecimiento y decrecimiento de la poblacion
! de humanos, zombies, y muertos. Dependiendo de unos parametros, los resultados seran diferentes. CAMBIAR EN zombies.f95.
! Nuestra version usa grandes ataques contra zombies, que se pueden introducir manualmente o mediante una plantilla ya hecha.


program main
use zombies
use euler
use files
implicit none

integer :: ataques, i
real*8 :: S,Z,R, k, deltaZ, PARAM(5)
real*8, allocatable  :: TIEMPOS(:)

! ==========================
! VALORES INICIALES
! ==========================

! PARAM.conf e INIT.conf
call leer_parametros(PARAM,k)
call leer_condiciones_iniciales(S,Z,R)

! ==========================
! CONFIGUTRACION DE LOS ATAQUES
! ==========================

write(*,*) "¿Desea usar una plantilla preestablecida de ataques? 0=Si, Cualquier otro entero=No"
read(*,*)  i

if (i==0) then
    ataques = 4
    allocate(TIEMPOS(0:ataques))
    TIEMPOS(0) = 0.d0
    TIEMPOS(1) = 60
    TIEMPOS(2) = 90
    TIEMPOS(3) = 120
    TIEMPOS(4) = 200
else
    write(*,*) "¿Cuantos ataques programados a los zombies desea?"
    read(*,*) ataques
    allocate(TIEMPOS(0:ataques))

    TIEMPOS(0) = 0.d0
    do i=1, ataques
        write(*,*) "¿Que tiempo desea para el ataque numero", i
        read(*,*) TIEMPOS(i)
    end do
end if

! Reset a los archivos que almacenan los datos  de la resolucion, ya que si no se añadirian a los de una ejecucion anterior
call create_and_clean

! ==========================
! RESOLUCION DEL SISTEMA DE ECUACIONES DIFERECIALES
! ==========================

do i=1, ataques
    ! Backward Euler
    call resolver_EDO_backward(s_prima,z_prima,r_prima,s_prima2,z_prima2,r_prima2,&
    & S,Z,R,TIEMPOS(i-1),TiEMPOS(i),PARAM)

    ! Los ataques aumentan en intensidad con el tiempo, con k y con el numero de personas
    deltaZ = k*(S**(0.1))*i*Z

    ! Failsafe para evitar que el numero de zombies sea negativo
    if ((Z-deltaZ)<0.d0) then 
        R = R + Z
        Z = 0
    else 
        Z = Z - deltaZ
        R = R + deltaZ
    endif

end do

write(*,*) "Zombies finales:", Z
write(*,*) "Humanos finales:", S
write(*,*) "Retirados finales:", R

deallocate(TIEMPOS)
end program main