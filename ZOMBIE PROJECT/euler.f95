module euler
use files
contains

subroutine resolver_EDO_backward(DS,DZ,DR,DS2,DZ2,DR2,S0,Z0,R0,TInicial,TFinal,K)
    implicit none
    real*8, intent(inout) :: S0, R0, Z0 
    real*8, intent(in)    :: TInicial, TFinal, K(5)
    interface
        function DS(S,Z,PARAM)
            real*8 :: S,Z,PARAM(5)
            real*8 :: DS
        end function
        function DZ(S,Z,R,PARAM)
            real*8 :: S,Z,R,PARAM(5)
            real*8 :: DZ
        end function
        function DR(S,Z,R,PARAM)
            real*8 :: S,Z,R,PARAM(5)
            real*8 :: DR
        end function
        function DS2(Z,PARAM)
            real*8 :: Z,PARAM(5)
            real*8 :: DS2
        end function
        function DZ2(S,PARAM)
            real*8 :: S,PARAM(5)
            real*8 :: DZ2
        end function
        function DR2(PARAM)
            real*8 :: PARAM(5)
            real*8 :: DR2
        end function
    end interface
    
    ! Variables auxiliares
    integer :: i, j, N, max_iter
    ! Variables usadas en la resolucion de Euler
    real*8  :: S, Z, R, t
    ! Variables usadas en cada uno de los 3 resolucion por Newton-Raphson de las ecuaciones no lineales
    real*8  :: G, GPRIMA, tol
    ! Variables con componentes en cada uno de los dos anteriores: 1 en Euler, 2 en Newton
    real*8  :: incremento(2), S_prev(2), Z_prev(2), R_prev(2)

    ! =================================
    ! CONDICIONES INICIALES
    ! =================================

    ! Parametros para las resoluciones, cambiar si se desean mas resolcion/tolerancia
    incremento(1) = 0.01d0
    incremento(2) = 0.001d0
    max_iter      = 100000 ! Normalmente se saldra con la tolerancia
    tol           = 0.001d0

    ! Con access='append' se unen los resultados a los del ataque anterior
    open(unit=13,file='T_SRZ.dat',status='old',action='write',Access='append')

    S = S0
    Z = Z0
    R = R0
    S_prev = S0
    Z_prev = Z0
    R_prev = R0

    t = TInicial

    write(13,*) t,S,R,Z

    ! =================================
    ! ITERACIONES DE EULER
    ! =================================
    do while(t < TFinal)
        
        ! Metodo de Euler que resuelve G(S) = S - incremento*DS(S,Z) - S_anterior(1)

        !S = 0.d0
        !S_prev(2) = S
        do j=1, max_iter
            G = S_prev(2) - incremento(2)*DS(S_prev(2), Z_prev(1),K) - S_prev(1)
            GPRIMA = 1 - incremento(2)*DS2(Z_prev(1),K)

            S = S_prev(2) - G/GPRIMA

            if( (abs(S-S_prev(2)) < tol) .AND. ( abs(G)<tol)) exit
            S_prev(2) = S
        end do

        !Z = 0.d0
        !Z_prev(2) = Z
        do j=1, max_iter
            G =  Z_prev(2) - incremento(2)*DZ(S_prev(1), Z_prev(2),R_prev(1),K) - Z_prev(1)
            GPRIMA = 1 - incremento(2)*DZ2(S_prev(1),K)

            Z = Z_prev(2) - G/GPRIMA

            if( (abs(Z-Z_prev(2)) < tol) .AND. ( abs(G)<tol)) exit
            Z_prev(2) = Z
        end do

        !R = 0.d0
        !R_prev(2) = R
        do j=1, max_iter
            G =  R_prev(2) - incremento(2)*DR(S_prev(1), Z_prev(1),R_prev(2),K) - R_prev(1)
            GPRIMA = 1 - incremento(2)*DR2(K)

            R = R_prev(2) - G/GPRIMA

            if( (abs(R-R_prev(2)) < tol) .AND. ( abs(G)<tol)) exit
            R_prev(2) = R
        end do

        t = t + incremento(1)

        write(13,*) t,S,R,Z

        S_prev = S
        Z_prev = Z
        R_prev = R

    end do
    close(13)

    ! Se almacenan los valores finales en las variables [S,Z,R]0, ya que son inout
    S0 = S
    Z0 = Z
    R0 = R
    
end subroutine

end module
