subroutine auto_potencia (A,AUTOVALOR,N)
    implicit none
    ! Variables de entrada/salida
    integer, intent(in) :: N
    real*8, intent(in)  :: A(N,N)
    real*8, intent(out) :: AUTOVALOR
    ! Variables propias
    real*8 :: Q(N)

    Q=(1.d0)/sqrt(1.d0*N)
    write(*,*) Q ! DEBUG
end subroutine auto_potencia