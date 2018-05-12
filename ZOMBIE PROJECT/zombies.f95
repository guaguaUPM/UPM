module zombies
! Este modulo contiene las funciones en forma analitica del modelo que estamos usando.
! S = Numero de susceptibles a convertirse en zombies
! Z = Numero de Zombies
! R = Numero de muertos
! (Con prima se indica que es la derivada, el ratio de incremento de estas variables)
! 
! Tambien se usan cuatro variables que pueden cambiarse que definen el comportamiento durante el tiempo:
! PI    = Ratio de nacimientos (0 a corto plazo)
! ALPHA = Ratio de muerte de zombies eliminados por los humanos
! BETA  = Ratio de transmision de humano a zombie
! CHI   = Ratio de resurrecion de muerto a zombie
! DELTA = Ratio de muerte humana por otras causas


implicit none
real*8:: PI    = 0.0d0    ,&
                    ALPHA = 7.5d-3 ,&
                    BETA  = 5.5d-3 ,&
                    CHI   = 9.d-2    ,&
                    DELTA = 1.0d-4
contains 

    function s_prima(S,Z)
        implicit none
        real*8 :: S, Z, s_prima

        s_prima = PI - BETA*S*Z - DELTA*S
    end function s_prima

    function z_prima(S,Z,R)
        implicit none
        real*8 :: z_prima, S,Z,R

        z_prima = BETA*S*Z + CHI*R - ALPHA*S*Z
    end function

    function r_prima(S,Z,R)
        implicit none
        real*8 :: r_prima, S,Z,R

        r_prima = DELTA*S + ALPHA*S*Z - CHI*R
    end function


    ! d(DS)/dS, d(DZ)/dZ, d(DR)/dR; para backward euler
    function s_prima2(Z)
        implicit none
        real*8 :: Z, s_prima2

        s_prima2 = - BETA*Z - DELTA
    end function s_prima2

    function z_prima2(S)
        implicit none
        real*8 :: S, z_prima2

        z_prima2 = BETA*S - ALPHA*S
    end function z_prima2

    function r_prima2()
        implicit none
        real*8 :: r_prima2

        r_prima2 = -CHI
    end function r_prima2

end module
