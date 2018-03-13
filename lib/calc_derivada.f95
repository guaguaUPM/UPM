subroutine derivada(FUNCION,A,VALOR)
    implicit none
    interface
        function FUNCION(X)
            real*8 :: X
            real*8 :: FUNCION
        end function
    end interface
    real*8, intent(in) :: A
    real*8, intent(out) :: VALOR

    write(*,*) FUNCION(A)
    VALOR = 2.d0

end subroutine derivada