module euler
contains
subroutine resolver_EDO(DS,DZ,DR,S0,Z0,R0,TInicial,TFinal)
    implicit none
    real*8, intent(inout) :: S0, R0, Z0 
    real*8, intent(in)  :: TInicial, TFinal
    interface
        function DS(S,Z)
            real*8 :: S,Z
            real*8 :: DS
        end function
    end interface

    interface
        function DZ(S,Z,R)
            real*8 :: S,Z,R
            real*8 :: DZ
        end function
    end interface

    interface
        function DR(S,Z,R)
            real*8 :: S,Z,R
            real*8 :: DR
        end function
    end interface
    
    integer :: i, N
    real*8  :: S,Z,R, incremento, t, S_anterior, Z_anterior, R_anterior

    incremento = 0.01d0

    open(unit=11,file='T_S.dat',status='old',action='write',Access='append')
    open(unit=12,file='T_Z.dat',status='old',action='write',Access='append')
    open(unit=13,file='T_R.dat',status='old',action='write',Access='append')

    S = S0
    Z = Z0
    R = R0

    S_anterior = S0
    Z_anterior = Z0
    R_anterior = R0

    t = TInicial


    write(11,*) t,S
    write(12,*) t,Z
    write(13,*) t,R
    do while (t<TFinal)
         
        S = DS(S_anterior,Z_anterior) * incremento + S_anterior
        Z = DZ(S_anterior,Z_anterior,R_anterior) * incremento + Z_anterior
        R = DR(S_anterior,Z_anterior,R_anterior) * incremento + R_anterior
        
        t = t + incremento

        write(11,*) t,S
        write(12,*) t,Z
        write(13,*) t,R

        S_anterior = S
        Z_anterior = Z
        R_anterior = R


    end do
    close(11)
    close(12)
    close(13)

    S0 = S
    Z0 = Z
    R0 = R

end subroutine





subroutine resolver_EDO_backward(DS,DZ,DR,DS2,DZ2,DR2,S0,Z0,R0,TInicial,TFinal)
    implicit none
    real*8, intent(inout) :: S0, R0, Z0 
    real*8, intent(in)  :: TInicial, TFinal
    interface
        function DS(S,Z)
            real*8 :: S,Z
            real*8 :: DS
        end function
        function DZ(S,Z,R)
            real*8 :: S,Z,R
            real*8 :: DZ
        end function
        function DR(S,Z,R)
            real*8 :: S,Z,R
            real*8 :: DR
        end function

        function DS2(Z)
            real*8 :: Z
            real*8 :: DS2
        end function
        function DZ2(S)
            real*8 :: S
            real*8 :: DZ2
        end function
        function DR2()
            real*8 :: DR2
        end function
    end interface
        
    integer :: i,j, N, max_iter
    real*8  :: S,Z,R, incremento(2), t, S_prev(2), Z_prev(2), R_prev(2)
    real*8  :: G, GPRIMA, tol

    ! La componente 1 es para las iteraciones de Backward y la 2 para la resolucion de Newton
    
    incremento(1) = 0.01d0
    incremento(2) = 0.001d0
    max_iter = 100000
    tol = 0.001
    
    open(unit=11,file='T_S.dat',status='old',action='write',Access='append')
    open(unit=12,file='T_Z.dat',status='old',action='write',Access='append')
    open(unit=13,file='T_R.dat',status='old',action='write',Access='append')
    
    S = S0
    Z = Z0
    R = R0

    S_prev = S0
    Z_prev = Z0
    R_prev = R0

    t = TInicial

    write(11,*) t,S
    write(12,*) t,Z
    write(13,*) t,R

    do while(t < TFinal)
        
        ! Metodo de Euler que resuelve G(S) = S - incremento*DS(S,Z) - S_anterior(1)

        do j=1, max_iter
            ! S_anterior(2) - G(S_anterior(2)) / G'(S_anterior(2)
            G = S_prev(2) - incremento(2)*DS( S_prev(2), Z_prev(1)) - S_prev(1)
            GPRIMA = 1 - incremento(2)*DS2(Z_prev(1))

            S = S_prev(2) - G/GPRIMA

            if( (abs(S-S_prev(2)) < tol) .AND. ( abs(G)<tol)) exit
            S_prev(2) = S
        end do

        do j=1, max_iter
            G =  Z_prev(2) - incremento(2)*DZ( S_prev(1), Z_prev(2),R_prev(1)) - Z_prev(1)
            GPRIMA = 1 - incremento(2)*DZ2(S_prev(1))

            Z = Z_prev(2) - G/GPRIMA

            if( (abs(Z-Z_prev(2)) < tol) .AND. ( abs(G)<tol)) exit
            Z_prev(2) = Z
        end do

        do j=1, max_iter
            G =  R_prev(2) - incremento(2)*DR( S_prev(1), Z_prev(1),R_prev(2)) - R_prev(1)
            GPRIMA = 1 - incremento(2)*DR2()

            R = R_prev(2) - G/GPRIMA

            if( (abs(R-R_prev(2)) < tol) .AND. ( abs(G)<tol)) exit
            R_prev(2) = R
        end do

        t = t + incremento(1)

        write(11,*) t,S
        write(12,*) t,Z
        write(13,*) t,R

        S_prev = S
        Z_prev = Z
        R_prev = R

    end do
    
end subroutine

end module
