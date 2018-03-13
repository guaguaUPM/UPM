subroutine auto_potencia (A,AUTOVECTOR,TOL,N)
    implicit none
    ! Variables de entrada/salida
    integer, intent(in) :: N
    real*8, intent(in)  :: A(N,N), TOL
    real*8, intent(out) :: AUTOVECTOR(N)

    ! Variables propias
    real*8 :: Q(N), Q_ANTERIOR(N), norma, resto(N)
    integer :: i, j, maxiter

    maxiter = 10

    do i = 1, maxiter
        Q=(1.d0)
        call norma2(norma,Q,N)
        Q = Q/norma
        write(*,*) Q

        Q_ANTERIOR = Q

        Q = matmul(A,Q)
        call norma2(norma,Q,N)
        Q = Q/norma
        write(*,*) Q
        
        resto = Q - Q_ANTERIOR

        if( maxval(resto) <tol) then
            AUTOVECTOR = Q
            return
        endif
    enddo

end subroutine auto_potencia