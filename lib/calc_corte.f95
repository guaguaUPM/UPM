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
    do
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

subroutine corte_newton_anal(f,df,x,tol,max_iter,corte)
    real*8,intent(in) :: x 
    real*8,intent(in) :: tol 
    integer,intent(in) :: max_iter
    interface
        function f(x)
            real*8 :: x 
            real*8 :: f
        end function
        function df(x)
            real*8 :: x 
            real*8 :: df
        end function
    end interface   
    real*8 :: x0,x1
    x0 = x 
    x1 = x
    do iter = 1, max_iter
    x1 = x0 - (f(x0))/(df(x0))   ! punto medio del intervalo
    if((abs((x-x0)/x1)<tol).and.(abs(f(x1))<tol))  exit ! control de error
    x0 = x1
    write(*,*) x1
    enddo
    X1 = corte 
end subroutine corte_newton_anal
    
    
