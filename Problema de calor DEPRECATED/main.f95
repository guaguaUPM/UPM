! CONSISTE EN RESOLVER UNA ECUACIÓN DIFERENCIAL DEL TIPO U¨¨(X) = 0 A TRAVES DE UN PROBLEMA DE CONTORNO
! SE EMPLEA LA MATRIZ DE CONTORNO AL CUADRADO (DERIVADA SEGUNDA) Y SE RESUELVE EL SISTEMA POR EL METODO
! MAS EFICAZ
program calor
implicit none

real*8, allocatable :: CONT1(:,:), CONT2(:,:),cont3(:,:), MATPROBLEMA(:), Xfinal(:), Identidad(:,:),Identidad2(:,:)
real*8              :: T1, T2 !T1 corresponde a la temperatura en el extremo izq de la barra, T2 a la del extremo derecho de la barra
real*8              :: k_Al, k,k_Ac
integer             :: N,i,j
logical             :: abrazafarolas

k_Al = 209.3d0 !W/mK

k_Ac = 16.3d0 !W/mK
! PARA MH COGER UNA T3 DIVISION AC Y AL Y IGUALAR LOS PROBLEMAS DE CONTORNO
T1 = 500.0d0 !K
T2 = 298.15d0 !K

write(*,*) "Introduzca el numero de particiones deseado para la función T"
read(*,*) N

allocate(CONT1(n,n))
allocate(CONT2(n,n))
allocate(CONT3(n,n))
allocate(MATPROBLEMA(N))
allocate(Xfinal(N))
allocate(Identidad(N,N))
allocate(Identidad2(N,N))
! CALCULO DE PARCIAL DE T RESPECTO X (T´=MCONT*T)
call matrizcontorno(CONT1, N, T1, T2)
!call write_A(Cont1,N)



!write(*,*) "Matriz contorno 1 arriba 2 abajo"
! CALCULO DE PARCIAL DE K*T´ RESPECTO DE X [(K*T´)´= MCONT*(K*T´)]
Identidad=0
do i=1,n
  if (i <= n/2 ) then
    Identidad(i,i) = k_Ac
  else 
    Identidad(i,i) = k_Al
  endif
enddo
!call write_A(Identidad,n)



cont3 = matmul(identidad,cont1)
!call write_A(cont3,n)

cont2 = matmul(cont1,cont3)

! MATRIZ PROBLEMA (El extremo esta en la temperatura del horno, el resto esta a temp ambiente)

MATPROBLEMA = 0
MATPROBLEMA(1)= T1
MATPROBLEMA(N)= T2

!call write_A(Cont2,N)

cont2(1,:) = 0
cont2(1,1) = 1
cont2(n,:) = 0
cont2(n,n) = 1
!call write_AB(Cont2,MATPROBLEMA,N)


call matrizT_gauss_seidel (CONT2, Identidad2, N)
call convergencia (Identidad2, abrazafarolas, N)
write(*,*) abrazafarolas


!Resolver sistema
!call resolver_gauss_seidel_iter(CONT2,Xfinal,MATPROBLEMA,N,1000)
!call resolver_lapack(Cont2,MATPROBLEMA,Xfinal,N)
!call resolver_LU(Cont2,MATPROBLEMA,Xfinal,N)
!write(*,*) "Solucion"
!write(*,*) Xfinal
if (abrazafarolas) then
  call resolver_jacobi_tol (CONT2, Xfinal, MATPROBLEMA, sqrt(epsilon(T1)), N)
else 
  write(*,*) "No converge por iterativos, usando LU"
  call resolver_LU(Cont2,MATPROBLEMA,Xfinal,N)
end if




!DEBUG MATRIZ CONTORNO  
!x1=10.d0
!x2=20.d0
!allocate(matriz(n,n))
!call matrizcontorno(matriz, n, x1, x2)
!call write_A(matriz, n)

! vvv HECHO POR FER vvv
! Hace falta tener el matplotlib instalado. Ver puntos.dat para comprobar.
! IMPORTANTE: La grafica quedará con la T menor a la izquierda, aunque este declarada como T2
write(*,*) "¿Representar? Si: 0, No: cualquier otro entero"
read(*,*) i
if(i==0) then
  call datos_a_puntos(Xfinal,N)
  call SYSTEM("python plot.py")
end if

end program calor