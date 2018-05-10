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
end module
