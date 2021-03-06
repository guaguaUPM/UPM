! CONSISTE EN RESOLVER UNA ECUACIÓN DIFERENCIAL DEL TIPO U¨¨(X) = 0 A TRAVES DE UN PROBLEMA DE CONTORNO
! SE EMPLEA LA MATRIZ DE CONTORNO AL CUADRADO (DERIVADA SEGUNDA, TENIENDO EN CUENTA LA K(X)) Y SE RESUELVE
! EL SISTEMA POR EL METODO MAS EFICAZ
program calor
use puntos
use lib_extra
use lib_iterativos
use lib_LU
use lib_gauss

implicit none

real*8, allocatable :: CONTORNO1(:,:), CONTORNO2(:,:), B(:), TEMPERATURA(:), MATRIZ_K(:,:), matriz_t(:,:), matriz_t2(:,:)
real*8              :: T1, T2, k_Al, k_ACERO, tol_iterativos
! T1 corresponde a la temperatura en el extremo izq de la barra
! T2 a la del extremo derecho de la barra
integer             :: N, i, j
logical             :: converge, converge2, pivotarsi, diagfilas, diagcol

k_Al = 209.3d0   !W/mK
k_ACERO = 16.3d0 !W/mK

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
allocate(matriz_t2,  mold=CONTORNO1)
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

! VECTOR PROBLEMA
B = 0.d0
B(1)= T1
B(N)= T2

! PARTICULARIZAMOS LA MATRIZ FINAL
CONTORNO2(1,:) = 0.d0
CONTORNO2(1,1) = 1.d0
CONTORNO2(n,:) = 0.d0
CONTORNO2(n,n) = 1.d0

! RESOLUCION FINAL
write(*,*) "¿Que metodo desea para resolver el sistema?"
write(*,*)
write(*,*) "1: Gauss-Seidel comprobando antes la convergencia"
write(*,*) "2: Jacobi comprobando antes la convergencia"
write(*,*) "3: Factorizacion LU sin pivote"
write(*,*) "4: Gauss"
write(*,*) "Cualquier otro entero: Iterativos si posible, LU en caso negativo y Gauss en caso de pivote necesario"
write(*,*)
read(*,*) i

select case(i)
  case(1)
    call convergencia_diag_col(CONTORNO2,diagcol,N)
    call convergencia_diag_filas(CONTORNO2,diagfilas,N)
    if (diagcol .or. diagfilas) then
      write(*,*) "Su matriz es diagonalmente predominante, convergencia asegurada. RESOLVIENDO"
      call resolver_gauss_seidel_tol(CONTORNO2, TEMPERATURA, B, tol_iterativos, N)
    else
      write(*,*) "Comprobando convergencia..."
      call matrizT_gauss_seidel(CONTORNO2, matriz_t, N)
      call convergencia_pot(matriz_t, converge, N)
      if (converge) then
        write(*,*) "Resolviendo"
        call resolver_gauss_seidel_tol(CONTORNO2, TEMPERATURA, B, tol_iterativos, N)
      else
        write(*,*) "Su matriz no converge, escoja otro metodo"
        return
      end if
    end if

  case(2)
    call convergencia_diag_col(CONTORNO2,diagcol,N)
    call convergencia_diag_filas(CONTORNO2,diagfilas,N)
    if (diagcol .or. diagfilas) then
      write(*,*) "Su matriz es diagonalmente predominante, convergencia asegurada. RESOLVIENDO"
      call resolver_jacobi_tol(CONTORNO2, TEMPERATURA, B, tol_iterativos, N)
    else
      write(*,*) "Comprobando convergencia..."
      call matrizT_jacobi(CONTORNO2, matriz_t, N)
      call convergencia_pot(matriz_t, converge, N)
      if (converge) then
        write(*,*) "Resolviendo"
        call resolver_jacobi_tol(CONTORNO2, TEMPERATURA, B, tol_iterativos, N)
      else
        write(*,*) "Su matriz no converge, escoja otro metodo"
        return
      end if
    end if

  case(3)
    call resolver_LU(CONTORNO2,B,TEMPERATURA,N)

  case(4)
    call resolver_gauss(CONTORNO2,B,TEMPERATURA,N)

  case default
    call convergencia_diag_col(CONTORNO2,diagcol,N)
    call convergencia_diag_filas(CONTORNO2,diagfilas,N)
    if (diagcol .or. diagfilas) then
      write(*,*) "Su matriz es diagonalmente predominante, convergencia asegurada. RESOLVIENDO"
      call resolver_gauss_seidel_tol(CONTORNO2, TEMPERATURA, B, tol_iterativos, N)
    else 
      write(*,*) "Comprobando convergencia..."
      call matrizT_gauss_seidel(CONTORNO2, matriz_t, N)
      call matrizT_jacobi(CONTORNO2, matriz_t2, N)
      call convergencia_pot(matriz_t, converge, N)
      call convergencia_pot(matriz_t2, converge2, N)
      do j=1, N
        if (CONTORNO2(i,i)==0.d0) pivotarsi=.true.
      end do
      if (converge) then 
        write(*,*) "Resolviendo por Gauss-Seidel"
        call resolver_gauss_seidel_tol(CONTORNO2, TEMPERATURA, B, tol_iterativos, N)
      else if (converge2) then
        write(*,*) "Resolviendo por Jacobi"
        call resolver_jacobi_tol(CONTORNO2, TEMPERATURA, B, tol_iterativos, N)
      else if (.not. pivotarsi) then
        write(*,*) "Metodos iterativos no posibles, resolviendo por LU"
        call resolver_LU(CONTORNO2,B,TEMPERATURA,N)
      else
        write(*,*) "Metodos iterativos ni LU posibles, resolviendo por Gauss con pivote"
        call resolver_gauss(CONTORNO2,B,TEMPERATURA,N)
      end if
    end if
end select


! Hace falta tener el matplotlib instalado. Ver puntos.dat para comprobar.
write(*,*) "¿Representar? Si: 0, No: cualquier otro entero"
read(*,*) i
if(i==0) then
  call datos_a_puntos(TEMPERATURA,N)
  call SYSTEM("python plot.py")
  write(*,*) "IMAGEN plot.png CREADA"
else
  write(*,*) TEMPERATURA
end if

end program calor