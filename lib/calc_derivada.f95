subroutine derivada1_prog1(FUNCION,ABCISA,M,INCREMENTO)
    implicit none
    interface
        function FUNCION(X)
            real*8 :: X
            real*8 :: FUNCION
        end function
    end interface
    real*8, intent(in) :: ABCISA, INCREMENTO
    real*8, intent(out) :: M

    M = (FUNCION(ABCISA+incremento)-FUNCION(ABCISA))/incremento

end subroutine derivada1_prog1

subroutine derivada1_prog2(FUNCION,ABCISA,M,INCREMENTO)
    implicit none
    interface
        function FUNCION(X)
            real*8 :: X
            real*8 :: FUNCION
        end function
    end interface
    real*8, intent(in) :: ABCISA, INCREMENTO
    real*8, intent(out) :: M

    M = (-FUNCION(ABCISA + incremento+2.d0*incremento) + 4.d0*FUNCION(ABCISA+incremento)-3*FUNCION(ABCISA))/(2.d0*incremento)
end subroutine derivada1_prog2

subroutine derivada1_regr1(FUNCION,ABCISA,M, INCREMENTO)
    implicit none
    interface
        function FUNCION(X)
            real*8 :: X
            real*8 :: FUNCION
        end function
    end interface
    real*8, intent(in) :: ABCISA, INCREMENTO
    real*8, intent(out) :: M
    
    M = (FUNCION(ABCISA) - FUNCION(ABCISA-incremento))/incremento
    
end subroutine derivada1_regr1

subroutine derivada1_cent1(FUNCION,ABCISA,M,INCREMENTO)
    implicit none
    interface
        function FUNCION(X)
            real*8 :: X
            real*8 :: FUNCION
        end function
    end interface
    real*8, intent(in) :: ABCISA, INCREMENTO
    real*8, intent(out) :: M
        
    M = (FUNCION(ABCISA+incremento) - FUNCION(ABCISA-incremento))/(2.d0*incremento)
        
end subroutine derivada1_cent1

subroutine derivada1_cent2(FUNCION,ABCISA,M,INCREMENTO)
    implicit none
    interface
        function FUNCION(X)
            real*8 :: X
            real*8 :: FUNCION
        end function
    end interface
    real*8, intent(in) :: ABCISA, INCREMENTO
    real*8, intent(out) :: M
            
    M = ( -FUNCION(ABCISA+2.d0*INCREMENTO) + 8.d0*FUNCION(ABCISA+INCREMENTO) - &
        8.d0*FUNCION(ABCISA-INCREMENTO) + FUNCION(ABCISA-2.d0*INCREMENTO) ) / (12.d0*INCREMENTO)
            
end subroutine derivada1_cent2