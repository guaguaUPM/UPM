module lib_extra
contains

subroutine matrizcontorno (MATRIZ, PARTICIONES, X1, X2)
  ! SUBRUTINA QUE CALCULA UNA MATRIZ DE CONTORNO GENERICA DADO EL
  ! NUMERO DE PARTICIONES EN LOS QUE SE QUIERA DISCRETIZAR NUESTRA VARIABLE
  ! Y EL TAMAÑO DEL INCREMENTO (DEDUCIDO A TRAVES DEL PUNTO INICIAL Y FINAL)
  implicit none

  ! ARGUMENTOS
  real*8, intent(in)  :: X1, X2
  integer, intent(in) :: PARTICIONES
  real*8, intent(out) :: MATRIZ(PARTICIONES,PARTICIONES)

  ! VARIABLES LOCALES
  real*8              :: incremento
  integer             :: i, j

  incremento = abs((X2-X1)/(PARTICIONES*1.d0))
  MATRIZ = 0.d0
  MATRIZ(1,2) = 1/(incremento*1.d0)
  MATRIZ(1,1) = -1/(incremento*1.d0)
  MATRIZ(PARTICIONES,PARTICIONES) = 1/(incremento*1.d0)
  MATRIZ(PARTICIONES,PARTICIONES-1) = -1/(incremento*1.d0)
  do i=2, PARTICIONES-1
      do j=2, PARTICIONES-1
          MATRIZ(i,i+1) = 1/(2.d0*incremento)
          MATRIZ(i,i-1) = -1/(2.d0*incremento)
      enddo
  enddo

end subroutine

end module lib_extra