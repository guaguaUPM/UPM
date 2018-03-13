subroutine auto_potencia (A,AUTOVALOR,TOL,N)
    implicit none
    ! Variables de entrada/salida
    integer, intent(in) :: N
    real*8, intent(in)  :: A(N,N), TOL
    real*8, intent(out) :: AUTOVALOR
    ! Variables propias
    real*8 :: Q(N), Q_ANTERIOR(N), error, norma, norma_anterior

    Q=(1.d0)
    call norma2(norma,Q,N)
    Q = Q/norma
    write(*,*) Q

    Q_ANTERIOR = Q

    Q = matmul(A,Q)
    call norma2(norma,Q,N)
    Q = Q/norma
    write(*,*) Q

end subroutine auto_potencia