module funciones
contains
    function parabola(X)
        real*8 :: X
        real*8 :: parabola

        parabola = X**2
    end function parabola
    function arcotangente(X)
        real*8 :: X
        real*8 :: arcotangente
    
        arcotangente = ATAN(X)
    end function arcotangente
    function paraboloide(X,Y)
        real*8 :: X,Y
        real*8 :: paraboloide

        paraboloide = X**2 + Y**2 + 2
    end function paraboloide
    function campana(X)
        real*8 :: X
        real*8 :: campana
      
        campana = exp(-(X**2))
    end function campana
    function campana_gorda(X,Y)
        real*8 :: X, Y
        real*8 :: campana_gorda
        
        campana_gorda = exp(-(X**2) -(Y**2))
    end function campana_gorda
    function TERMO(X)
        real*8 :: X
        real*8 :: TERMO

        TERMO = 0.0044571857150 * exp(3722.5870242822825/(X*1.d0))
    end function TERMO
end module funciones