module funciones
contains
    function exponencial(X)
        real*8 :: X
        real*8 :: exponencial

        exponencial = exp(X)
    end function exponencial
end module funciones