module zombies
! Este modulo contiene las funciones en forma analitica del modelo que estamos usando.
! S = Numero de susceptibles a convertirse en zombies
! Z = Numero de Zombies
! R = Numero de muertos
! (Con prima se indica que es la derivada, el ratio de incremento de estas variables)
! 
! Tambien se usan cinco variables que pueden cambiarse que definen el comportamiento durante el tiempo:
! 1. ALPHA = Ratio de muerte de zombies eliminados por los humanos
! 2. BETA  = Ratio de transmision de humano a zombie
! 3. DELTA = Ratio de muerte humana por otras causas
! 4. CHI   = Ratio de resurrecion de muerto a zombie
! 5. PI    = Ratio de nacimientos (0 a corto plazo)

implicit none
contains 

    function s_prima(S,Z,PARAM)
        implicit none
        real*8 :: S, Z, s_prima, PARAM(5)

        s_prima = PARAM(5) - PARAM(2)*S*Z - PARAM(3)*S
    end function s_prima

    function z_prima(S,Z,R,PARAM)
        implicit none
        real*8 :: z_prima, S,Z,R, PARAM(5)

        z_prima = PARAM(2)*S*Z + PARAM(4)*R - PARAM(1)*S*Z
    end function

    function r_prima(S,Z,R,PARAM)
        implicit none
        real*8 :: r_prima, S,Z,R, PARAM(5)

        r_prima = PARAM(3)*S + PARAM(1)*S*Z - PARAM(4)*R
    end function


    ! d(DS)/dS, d(DZ)/dZ, d(DR)/dR; para backward euler
    function s_prima2(Z,PARAM)
        implicit none
        real*8 :: Z, s_prima2, PARAM(5)

        s_prima2 = - PARAM(2)*Z - PARAM(3)
    end function s_prima2

    function z_prima2(S,PARAM)
        implicit none
        real*8 :: S, z_prima2, PARAM(5)

        z_prima2 = PARAM(2)*S - PARAM(1)*S
    end function z_prima2

    function r_prima2(PARAM)
        implicit none
        real*8 :: r_prima2, PARAM(5)

        r_prima2 = -PARAM(4)
    end function r_prima2

end module
