module funciones
contains
    function exponencial(X)
        real*8 :: X
        real*8 :: exponencial

        exponencial = exp(X)
    end function exponencial
    function coseno(X)
        real*8 :: X
        real*8 :: coseno
        coseno = cos(X)
    end function coseno
end module funciones