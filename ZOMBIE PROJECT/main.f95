! Mayo 2018
! Trabajo hecho por el Grupo 3 de Informática del M6.
! Integrantes:
!  Fernando Ayats Llamas
!  Ale
!  Fran
!  Alv
!  Lucs 

! A partir de un modelo de ecuaciones diferenciales, se simula un apocalipsis zombie y el crecimiento y decrecimiento de la poblacion
! de humanos, zombies, y muertos. Dependiendo de unos parametros, los resultados seran diferentes. CAMBIAR EN zombies.f95.
! Nuestra version usa grandes ataques contra zombies, que se pueden introducir manualmente o mediante una plantilla ya hecha.

! NOTA
! El TIEMPO hemos elegido medirlo en AÑOS
! El NUMERO de zombies, humanos y retirados no tienen ningun factor de conversion, es decir S=500 son 500 humanos.


program main
use zombies
use euler
use clean
implicit none

integer :: ataques, i
real*8 :: S,Z,R, k, deltaZ
real*8, allocatable  :: TIEMPOS(:)

! ==========================
! VALORES INICIALES

! Numero de Zombies, Susceptibles y Retirados al inicio
S = 500.d0
Z = 0.d0
R = 0.d0

! Factor que multiplica al numero de zombies elimindaos con cada ataque
k = 0.25d0


! ==========================
! CONFIGUTRACION DE LOS ATAQUES
write(*,*) "¿Desea usar una plantilla preestablecida de ataques? 0=Si, Cualquier otro entero=No"
read(*,*)  i

if (i==0) then
    ataques = 4
    allocate(TIEMPOS(0:ataques))
    TIEMPOS(0) = 0.d0
    TIEMPOS(1) = 25
    TIEMPOS(2) = 50
    TIEMPOS(3) = 60
    TIEMPOS(4) = 80
else
    write(*,*) "¿Cuantos ataques a los zombies quieres?"
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
do i=1, ataques
    call resolver_EDO(s_prima, z_prima, r_prima,S,Z,R,TIEMPOS(i-1),TiEMPOS(i))

    ! Los ataques aumentan en intensidad con el tiempo y con k
    deltaZ = k*i*Z
    Z = Z - deltaZ
    R = R + deltaZ
end do

end program main