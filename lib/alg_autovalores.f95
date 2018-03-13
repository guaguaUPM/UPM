subroutine auto_potencia (A,AUTOVALOR,TOL,N)
    implicit none
    ! Variables de entrada/salida
    integer, intent(in) :: N
    real*8, intent(in)  :: A(N,N), TOL
    real*8, intent(out) :: AUTOVALOR
    ! Variables propias
    real*8 :: Q(N), Q_ANTERIOR(N), error, norma, norma_anterior

    Q=(1.d0)/sqrt(1.d0*N)
    !do while(error > tol)
        write(*,*) Q ! DEBUG
        Q_ANTERIOR = Q
        norma_anterior = norma
    
        Q = matmul(A,Q)
        call norma2(norma,Q,N)
        Q = Q / norma
        error  = norma_anterior - norma
        write(*,*) error
    !end do
end subroutine auto_potencia