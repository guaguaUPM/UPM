!REQUISITOS: A MATRIZ CUADRADA, TODOS LOS AUTOVALORES DE A REALES Y ESTOS SON UN CONJUNTO ORDENADO
subroutine auto_potencia (A,AUTOVECTOR,TOL,N)
    implicit none
    ! Variables de entrada/salida
    integer, intent(in) :: N
    real*8, intent(in)  :: A(N,N), TOL
    real*8, intent(out) :: AUTOVECTOR(N)

    ! Variables propias
    real*8 :: Q(N), Q_ANTERIOR(N), norma, resto(N)
    integer :: i, maxiter

    maxiter = 10

    !   SELECCIONAR UN q0
    Q=(1.d0)
    call norma2(norma,Q,N)
    Q = Q/norma
    write(*,*) Q

    !   CALCULO DEL AUTOVECTOR
    do i = 1, maxiter

        resto = 0.d0
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
    
    !   CALCULO DEL AUTOVALOR ASOCIADO

end subroutine auto_potencia