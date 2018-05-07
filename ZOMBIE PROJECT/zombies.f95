module zombies
contains
    function s_prima(S,Z)
        implicit none
        real*8 :: S, Z, s_prima, PI, BETA, DELTA

        PI = 0.d0
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
