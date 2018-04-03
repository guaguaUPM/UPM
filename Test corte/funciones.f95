module funciones
contains
    function principal(X)
        real*8 :: X
        real*8 :: principal

        principal = X**3 - exp(1.d0)
    end function principal
    function secundaria(X)
        real*8 :: X
        real*8 :: secundaria

        secundaria = 3.d0 * x**2
    end function secundaria
end module funciones