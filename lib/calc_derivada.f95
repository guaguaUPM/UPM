subroutine derivada1_prog1(FUNCION,ABCISA,M)
    implicit none
    interface
        function FUNCION(X)
            real*8 :: X
            real*8 :: FUNCION
        end function
    end interface
    real*8, intent(in) :: ABCISA
    real*8, intent(out) :: M
    real*8 :: incremento
    incremento = EPSILON(ABCISA)
    incremento = SQRT(incremento)

    M = (FUNCION(ABCISA+incremento)-FUNCION(ABCISA))/incremento

end subroutine derivada1_prog1

subroutine derivada1_regr1(FUNCION,ABCISA,M)
    implicit none
    interface
        function FUNCION(X)
            real*8 :: X
            real*8 :: FUNCION
        end function
    end interface
    real*8, intent(in) :: ABCISA
    real*8, intent(out) :: M
    real*8 :: incremento
    incremento = EPSILON(ABCISA)
    incremento = SQRT(incremento)
    
    M = (FUNCION(ABCISA) - FUNCION(ABCISA-incremento))/incremento
    
end subroutine derivada1_regr1

subroutine derivada1_cent1(FUNCION,ABCISA,M)
    implicit none
    interface
        function FUNCION(X)
            real*8 :: X
            real*8 :: FUNCION
        end function
    end interface
    real*8, intent(in) :: ABCISA
    real*8, intent(out) :: M
    real*8 :: incremento
    incremento = EPSILON(ABCISA)
    incremento = SQRT(incremento)
        
    M = (FUNCION(ABCISA+incremento) - FUNCION(ABCISA-incremento))/(2.d0*incremento)
        
end subroutine derivada1_cent1