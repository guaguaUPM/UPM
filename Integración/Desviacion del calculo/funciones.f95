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
    function f001(X)
        real*8 :: X
        real*8 :: f001
      
        f001 = X*sin(X)
    end function f001
    function seno(X)
        real*8 :: X
        real*8 :: seno
      
        seno = sin(X)
    end function seno
    function campana(X)
        real*8 :: X
        real*8 :: campana
        
        campana = exp(-(X**2))
    end function campana
end module funciones