module newton
use funciones
implicit none

contains

subroutine newton_raphson_2D(X0, Y0, TOL, MAX_ITER, SOLUCION)
    implicit none
    real*8,intent(in)   :: X0, Y0, TOL
    integer,intent(in)  :: MAX_ITER
    real*8, intent(out) :: SOLUCION(2)

    real*8              :: solucion_anterior(2), f(2), j(2,2)
    integer             :: IPIV(2,2), info, i                 ! Variables dummy para la resulocion por LAPACK; auxiliar

    SOLUCION(1) = X0
    SOLUCION(2) = Y0

    do i = 1, MAX_ITER
        
        ! 1 --------------------
        ! Se resuelve J(Xi) * M = F(Xi)

        ! f(SOLUCION)
        call funcion(SOLUCION, f)
        ! j(SOLUCION)
        call jacobiano(SOLUCION, j)

        ! j(solucion) * Y = f(SOLUCION)
        ! f pasa a ser el valor de Y por el funcionamiento de dgesv
        call DGESV(2,1,j,2,IPIV,f,2,info)

        ! Se guarda el valor de Xn antes de conseguir el valor de Xn+1
        solucion_anterior = SOLUCION

        ! 2 --------------------
        ! Se evalua Xn+1 = = Xn - Yn. Recordar que f no es el valor de la funcion, sino la solucion de la ecuacion anterior
        SOLUCION = SOLUCION - f

        ! Calculo del error relativo ||Xn+1 - Xn||, que despejando es ||-Yn||, sin importar el signo
        if (sqrt(f(1)**2 + f(2)**2) <= TOL) then
            exit
        end if
    end do
end subroutine newton_raphson_2D

end module newton