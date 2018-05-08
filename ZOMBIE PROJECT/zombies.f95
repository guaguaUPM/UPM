module zombies
! Este modulo contiene las funciones en forma analitica del modelo que estamos usando.
! S = Numero de susceptibles a convertirse en zombies
! Z = Numero de Zombies
! R = Numero de muertos
! (Con prima se indica que es la derivada, el ratio de incremento de estas variables)
! 
! Tambien se usan cuatro variables que pueden cambiarse que definen el comportamiento durante el tiempo:
! PI    = Ratio de naciiento (0 a corto plazo)
! BETA  = Ratio de transmision
! DELTA = Ratio de muerte por muerte natural
! CHI   = Ratio de transformacion de humano a zombie

contains

    function s_prima(S,Z)
        implicit none
        real*8 :: S, Z, s_prima, PI, BETA, DELTA

        PI = 0.0d0
        BETA = 0.0055d0
        DELTA = 0.0001d0

        s_prima = PI - BETA*S*Z - DELTA*S
    end function s_prima

    function z_prima(S,Z,R)
        implicit none
        real*8 :: z_prima, S,Z,R,BETA,CHI,ALPHA

        BETA = 0.0055d0
        CHI = 0.09d0
        ALPHA = 0.0075d0

        z_prima = BETA*S*Z + CHI*R - ALPHA*S*Z
    end function

    function r_prima(S,Z,R)
        implicit none
        real*8 :: r_prima, S,Z,R, ALPHA, CHI, DELTA

        ALPHA = 0.0075d0
        CHI = 0.09d0
        DELTA = 0.0001d0

        r_prima = DELTA*S + ALPHA*S*Z - CHI*R
    end function
        
end module
