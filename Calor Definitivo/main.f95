! CONSISTE EN RESOLVER UNA ECUACIÓN DIFERENCIAL DEL TIPO U¨¨(X) = 0 A TRAVES DE UN PROBLEMA DE CONTORNO
! SE EMPLEA LA MATRIZ DE CONTORNO AL CUADRADO (DERIVADA SEGUNDA) Y SE RESUELVE EL SISTEMA POR EL METODO
! MAS EFICAZ
program calor
use puntos
use lib_extra
use lib_iterativos
use lib_LU

implicit none

real*8, allocatable :: CONTORNO1(:,:), CONTORNO2(:,:), B(:), TEMPERATURA(:), MATRIZ_K(:,:), matriz_t(:,:)
real*8              :: T1, T2, k_Al, k_ACERO, tol_iterativos
! T1 corresponde a la temperatura en el extremo izq de la barra
! T2 a la del extremo derecho de la barra
integer             :: N, i, j
logical             :: converge

k_Al = 209.3d0   !W/mK
k_ACERO = 16.3d0 !W/mK

! PARA MH COGER UNA T3 DIVISION AC Y AL Y IGUALAR LOS PROBLEMAS DE CONTORNO
T1 = 500.0d0  !K
T2 = 298.15d0 !K

! Tolerancia para usar en los metodos iterativos
tol_iterativos = 10.d-6

write(*,*) "Introduzca el numero de particiones deseado para la función T"
read(*,*) N

allocate(CONTORNO1(n,n))
allocate(CONTORNO2, mold=CONTORNO1)
allocate(MATRIZ_K,  mold=CONTORNO1)
allocate(matriz_t,  mold=CONTORNO1)
allocate(B(N))
allocate(TEMPERATURA, mold=B)


! CALCULO DE PARCIAL DE T RESPECTO X (T´=MCONT*T)
call matrizcontorno(CONTORNO1, N, T1, T2)

!write(*,*) "Matriz contorno 1 arriba 2 abajo"
! CALCULO DE PARCIAL DE K*T´ RESPECTO DE X [(K*T´)´= MCONT*(K*T´)]
MATRIZ_K=0.d0
do i=1,n
  if (i <= n/2 ) then
    MATRIZ_K(i,i) = k_ACERO
  else 
    MATRIZ_K(i,i) = k_Al
  endif
enddo
CONTORNO2 = matmul(CONTORNO1,matmul(MATRIZ_K,CONTORNO1))

! VECTOR PROBLEMA (El extremo esta en la temperatura del horno, el resto esta a temp ambiente)
B = 0.d0
B(1)= T1
B(N)= T2


CONTORNO2(1,:) = 0.d0
CONTORNO2(1,1) = 1.d0
CONTORNO2(n,:) = 0.d0
CONTORNO2(n,n) = 1.d0

! Resulcion final
write(*,*) "¿Que metodo desea para resolver el sistema?"
write(*,*)
write(*,*) "0: Gauss-Seidel"
write(*,*) "1: Gauss-Seidel comprobando antes la convergencia"
write(*,*) "3: Jacobi"
write(*,*) "4: Jacobi comprobando antes la convergencia"
write(*,*) "Cualquier otro entero: Factorizacion LU"
read(*,*) i

select case(i)
  case(0)
    call resolver_gauss_seidel_tol(CONTORNO2, TEMPERATURA, B, tol_iterativos, N)

  case(1)
    ! ARREGLAR
    call matrizT_gauss_seidel(CONTORNO2, matriz_t, N)
    call convergencia(matriz_t, converge, N)
    if (converge) then
      call resolver_gauss_seidel_tol(CONTORNO2, TEMPERATURA, B, tol_iterativos, N)
    else
      write(*,*) "NO CONVERGE"
      call exit
    end if

  case(2)
    call matrizT_jacobi(CONTORNO2, matriz_t, N)
    call convergencia(matriz_t, converge, N)
    if (converge) then
      call resolver_jacobi_tol(CONTORNO2, TEMPERATURA, B, tol_iterativos, N)
    else
      write(*,*) "NO CONVERGE"
      call exit
    end if

  case(3)
    call resolver_jacobi_tol(CONTORNO2, TEMPERATURA, B, tol_iterativos, N)

  case default
    call resolver_LU(CONTORNO2,B,TEMPERATURA,N)
end select


! Hace falta tener el matplotlib instalado. Ver puntos.dat para comprobar.
write(*,*) "¿Representar? Si: 0, No: cualquier otro entero"
read(*,*) i
if(i==0) then
  call datos_a_puntos(TEMPERATURA,N)
  call SYSTEM("python plot.py")
else
  write(*,*) TEMPERATURA
end if

end program calor