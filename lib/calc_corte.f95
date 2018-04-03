subroutine corte_biseccion(FUNCION,X1,X2,TOLERANCIA,CORTE)
    implicit none
    interface
        function FUNCION(X)
            real*8 :: X
            real*8 :: FUNCION
        end function
    end interface
    real*8, intent(inout) :: X1,X2
    integer, intent(in) :: TOLERANCIA
    real*8, intent(out) :: CORTE 
    real*8 :: x, valor
    integer::i
    do i=1,i+1
        x=(X1+X2)/2
        valor=abs(FUNCION(x))
        if (valor < 0) then 
            exit 
        elseif (FUNCION(x)*FUNCION(X1) < 0) then 
            X2=x 
        elseif (FUNCION(x)*FUNCION(X2) < 0) then
            X1=x
        endif
    enddo
        CORTE=x
    write(*,*) 'el numero de bisecciones es', i 
    write(*,*) 'la coordenada de corte es', corte
endsubroutine corte_biseccion 


    
    
