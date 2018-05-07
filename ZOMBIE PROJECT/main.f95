program main
use zombies
use euler
implicit none

integer :: ataques, i
real*8 :: S,Z,R, k
real*8, allocatable  :: TIEMPOS(:)

S = 500.d0
Z = 0.d0
R = 0.d0

k = 0.25d0

write(*,*) "¿Cuantos ataques a los zombies quieres?"
read(*,*) ataques
allocate(TIEMPOS(0:ataques))

TIEMPOS(0) = 0.d0
do i=1, ataques
    write(*,*) "¿Que tiempo desea para el ataque numero", i
    read(*,*) TIEMPOS(i)
end do

call SYSTEM("rm *.dat")
call SYSTEM("touch T_S.dat")
call SYSTEM("touch T_Z.dat")
call SYSTEM("touch T_R.dat")

do i=1, ataques
    call resolver_EDO(s_prima, z_prima, r_prima,200, S,Z,R,TIEMPOS(i-1),TiEMPOS(i))
    Z = Z - k*i*Z
end do

end program main