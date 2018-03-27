! CONSISTE EN RESOLVER UNA ECUACIÓN DIFERENCIAL DEL TIPO U¨¨(X) = 0 A TRAVES DE UN PROBLEMA DE CONTORNO
! SE EMPLEA LA MATRIZ DE CONTORNO AL CUADRADO (DERIVADA SEGUNDA) Y SE RESUELVE EL SISTEMA POR EL METODO
! MAS EFICAZ
program calor
implicit none

real*8, allocatable :: CONT1(:,:), CONT2(:,:), MATPROBLEMA(:,:)
real*8              :: T1, T2 !T1 corresponde a la temperatura en el extremo izq de la barra, T2 a la del extremo derecho de la barra
real*8              :: k_Al, k,Ac
integer             :: N

k_Al = 209.3d0 !W/mK
k_Ac = 16.3d0 !W/mK
! PARA MH COGER UNA T3 DIVISION AC Y AL Y IGUALAR LOS PROBLEMAS DE CONTORNO
T1 = 500.0d0 !K
T2 = 298.15d0 !K

write(*,*) "Introduzca el numero de particiones deseado para la función T"
read(*,*) N

! CALCULO DE PARCIAL DE T RESPECTO X (T´=MCONT*T)
call matrizcontorno(CONT1, N, T1, T2)

! CALCULO DE PARCIAL DE K*T´ RESPECTO DE X [(K*T´)´= MCONT*(K*T´)]
call matrizcontorno(CONT2, N, T1, T2)

! MATRIZ PROBLEMA














!DEBUG MATRIZ CONTORNO  
!x1=10.d0
!x2=20.d0
!allocate(matriz(n,n))
!call matrizcontorno(matriz, n, x1, x2)
!call write_A(matriz, n)

end program calor