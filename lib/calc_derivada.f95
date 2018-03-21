! Calculo de derivadas primeras y segundas segun varios metodos, siguiendo la regla:
! derivadaX_[modo]X (FUNCION, ABCISA, M, INCREMENTO)
!         ^       ^
!         |       Indica el orden del polinomio de Taylor usado (1 o 2)
!         Indica si la derivda es primera o segunda
! [modo] > prog
!        > regr
!        > cent (En teoría el mejor, pero mas cosotoso de los 3)
! FUNCION (real8 -> real8) Self explaintory
! ABCISA (real8) Punto x0 donde se evalua la derivda
! M (real8) es el valor de la derivada
! INCREMENTO valor de h en la expresion de la derivda. Cuanto menor, mejor la aproximacion, pero fluctua si es demsiado
!   pequeno. Al parecer SQRT( EPSILON(X0) ) funciona bien.


! --------------------
! PROGRESIVAS
! --------------------

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

subroutine derivada2_prog1(FUNCION,ABCISA,M,INCREMENTO)
    implicit none
    interface
        function FUNCION(X)
            real*8 :: X
            real*8 :: FUNCION
        end function
    end interface
    real*8, intent(in) :: ABCISA, INCREMENTO
    real*8, intent(out) :: M
    M = (FUNCION(ABCISA+2.d0*incremento) - 2.d0*FUNCION(ABCISA+incremento) + FUNCION(ABCISA))/(incremento)**2
end subroutine derivada2_prog1

subroutine derivada2_prog2(FUNCION,ABCISA,M,INCREMENTO)
    implicit none
    interface
        function FUNCION(X)
            real*8 :: X
            real*8 :: FUNCION
        end function
    end interface
    real*8, intent(in) :: ABCISA, INCREMENTO
    real*8, intent(out) :: M
    M = (-FUNCION(ABCISA+3.d0*incremento) + 4.d0*FUNCION(ABCISA+2.d0*incremento) - 5.d0*FUNCION(ABCISA+incremento) &
    + 2.d0*FUNCION(ABCISA))/(incremento)**2
end subroutine derivada2_prog2

! --------------------
! REGRESIVAS
! --------------------

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

subroutine derivada1_regr2(FUNCION,ABCISA,M, INCREMENTO)
    implicit none
    interface
        function FUNCION(X)
            real*8 :: X
            real*8 :: FUNCION
        end function
    end interface
    real*8, intent(in) :: ABCISA, INCREMENTO
    real*8, intent(out) :: M
    M = (FUNCION(ABCISA-2.d0*incremento) - 4.d0*FUNCION(ABCISA-incremento) + 3.d0*FUNCION(ABCISA))/(2.d0*incremento)
end subroutine derivada1_regr2

subroutine derivada2_regr1(FUNCION,ABCISA,M,INCREMENTO)
    implicit none
    interface
        function FUNCION(X)
            real*8 :: X
            real*8 :: FUNCION
        end function
    end interface
    real*8, intent(in) :: ABCISA, INCREMENTO
    real*8, intent(out) :: M
    M = (FUNCION(ABCISA-2.d0*incremento) - 2.d0*FUNCION(ABCISA-incremento) + FUNCION(ABCISA))/(incremento)**2
end subroutine derivada2_regr1
    
subroutine derivada2_regr2(FUNCION,ABCISA,M,INCREMENTO)
    implicit none
    interface
        function FUNCION(X)
            real*8 :: X
            real*8 :: FUNCION
        end function
    end interface
    real*8, intent(in) :: ABCISA, INCREMENTO
    real*8, intent(out) :: M
    M = (-FUNCION(ABCISA-3.d0*incremento) + 4.d0*FUNCION(ABCISA-2.d0*incremento) - 5.d0*FUNCION(ABCISA-incremento) &
    + 2.d0*FUNCION(ABCISA))/(incremento)**2
end subroutine derivada2_regr2

! --------------------
! CENTRADAS
! --------------------

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

subroutine derivada2_cent1(FUNCION,ABCISA,M,INCREMENTO)
    implicit none
    interface
        function FUNCION(X)
            real*8 :: X
            real*8 :: FUNCION
        end function
    end interface
    real*8, intent(in) :: ABCISA, INCREMENTO
    real*8, intent(out) :: M          
    M = (FUNCION(ABCISA+incremento) - 2.d0*FUNCION(ABCISA) + FUNCION(ABCISA-incremento))/(incremento)**2
end subroutine derivada2_cent1

subroutine derivada2_cent2(FUNCION,ABCISA,M,INCREMENTO)
    implicit none
    interface
        function FUNCION(X)
            real*8 :: X
            real*8 :: FUNCION
        end function
    end interface
    real*8, intent(in) :: ABCISA, INCREMENTO
    real*8, intent(out) :: M          
    M = (-FUNCION(ABCISA+2.d0*incremento) + 16.d0*FUNCION(ABCISA+incremento) - 30.d0*FUNCION(ABCISA) &
    + 16.d0*FUNCION(ABCISA-incremento) - FUNCION(ABCISA-2.d0*incremento))/(12.d0*incremento**2)
end subroutine derivada2_cent2