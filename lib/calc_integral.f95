! Calculo de integrales como suma de areas. Estas areas se van haciendo pequenas a medida que aumentan las particiones.
! FUNCION (real8 -> real8) Pues eso
! X1 (real8) Punto de la IZQUIERDA donde comenzar la integral
! X2 (real8) Punto de la DERECHA donde terminar la integral
! PARTICIONES (int) Numero de rectangulitos para ajustar la funcion. A partir de 300 el resultado suele ser bueno
! Area (real8) Salida, integral calculada

! Reimann -> Las areas son rectangulos con sus verices superior izq coincidentes con la funcion
! Trapecio -> Los rectangulos pasan continuamente unidos por lineas rectas (la que consigue una mejor aproximacion con menos calculos)
! Simpson -> Se interpola un polinomio en cada particion y se integra

subroutine integral_riemann(FUNCION,X1,X2,PARTICIONES,AREA)
    implicit none
    interface
        function FUNCION(X)
            real*8 :: X
            real*8 :: FUNCION
        end function
    end interface
    real*8, intent(in) :: X1,X2
    integer, intent(in) :: PARTICIONES
    real*8, intent(out) :: AREA

    real*8 :: incremento
    integer :: i

    incremento = (X2-X1)/(PARTICIONES*1.d0)
    AREA=0.d0
    do i = 0, PARTICIONES-1
        ! Se calula el area como suma de rectangulos, usando la altura como el valor de su vertice izquierdo superior
        AREA = AREA + incremento*FUNCION(X1+i*incremento)
    end do

end subroutine integral_riemann

subroutine integral_trapcio(FUNCION,X1,X2,PARTICIONES,AREA)
    implicit none
    interface
        function FUNCION(X)
            real*8 :: X
            real*8 :: FUNCION
        end function
    end interface
    real*8, intent(in) :: X1,X2
    integer, intent(in) :: PARTICIONES
    real*8, intent(out) :: AREA

    real*8 :: incremento
    integer :: i

    incremento = (X2-X1)/(PARTICIONES*1.d0)
    AREA=0.d0
    do i = 0, PARTICIONES-1
        ! Se calcula el area como suma de trapcios (una de sus caras es la media de las dos diferentes)
        AREA = AREA + incremento*(FUNCION(X1 + i*incremento) + FUNCION(X1 + (i+1)*incremento))/2.d0
    end do

end subroutine integral_trapcio

subroutine integral_simpson(FUNCION,X1,X2,P,AREA)
    implicit none
    interface
        function FUNCION(X)
            real*8 :: X
            real*8 :: FUNCION
        end function
    end interface
    real*8, intent(in) :: X1,X2
    integer, intent(in) :: P
    real*8, intent(out) :: AREA

    real*8 :: incremento, sum1, sum2
    integer :: i, PARTICIONES

    PARTICIONES = P
    if(mod(PARTICIONES,2) == 1) then
        PARTICIONES = PARTICIONES + 1
        write(*,*) "El metodo simpson necesita un numero par de particiones, se usarán", PARTICIONES
    end if

    ! Metodo copiado directamente de Wikipedia, funciono a la primera LUL
    incremento = (X2-X1)/(PARTICIONES*1.d0)
    sum1 = 0.d0
    do i = 1, PARTICIONES/2 - 1
        sum1 =  sum1 + FUNCION(X1 + (2*i)*incremento)
    end do
    sum2 = 0.d0
    do i = 1, PARTICIONES/2
        sum2 = sum2 + FUNCION(X1 + (2*i-1)*incremento)
    end do
    AREA = (incremento/3.d0) * ( FUNCION(X1) + 2*sum1 + 4*sum2 + FUNCION(X2))

end subroutine integral_simpson