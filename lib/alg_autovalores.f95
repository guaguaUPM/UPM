!REQUISITOS: A SEA MATRIZ CUADRADA, TODOS LOS AUTOVALORES DE A SEAN REALES 
!   Y QUE ESTOS SEAN UN CONJUNTO ORDENADO

subroutine auto_potencia (A, AUTOVALOR, TOL, Q0, N)
    !CALCULA EL AUTOVALOR MAS GRANDE EN VALOR ABSOLUTO
    implicit none

    ! Variables de entrada/salida
    integer, intent(in) :: N
    real*8, intent(in)  :: A(N,N), TOL, Q0(N)
    real*8, intent(out) :: AUTOVALOR

    ! Variables propias
    real*8 :: Q(N), Q_ANTERIOR(N), norma, resto(N), AUTOVECTOR(N)
    integer :: i, maxiter

    maxiter = 10

    !   SELECCIONAR UN q0
    !Q=(1.d0)

    Q = Q0
    call norma2(norma,Q,N)
    if (norma /= 1.d0) then
        Q = Q/norma
    endif
    write(*,*) Q

    !   CALCULO DEL AUTOVECTOR
    do i = 1, maxiter

        Q_ANTERIOR = Q

        Q = matmul(A,Q)
        call norma2(norma,Q,N)
        Q = Q/norma
        write(*,*) Q
        
        resto = Q - Q_ANTERIOR

        if( maxval(resto) <tol) then
            AUTOVECTOR = Q
            exit
        endif
    enddo
    
    !   CALCULO DEL AUTOVALOR ASOCIADO (coef. de Rayleigh)
    AUTOVALOR = DOT_PRODUCT(AUTOVECTOR, matmul(A, AUTOVECTOR)) / DOT_PRODUCT(AUTOVECTOR, AUTOVECTOR)

end subroutine auto_potencia

subroutine auto_potencia_inversa (A, AUTOVALOR, TOL, Q0, N)
    !CALCULA EL AUTOVALOR MAS PEQUEÑO EN VALOR ABSOLUTO
    implicit none

    ! Variables de entrada/salida
    integer, intent(in) :: N
    real*8, intent(in)  :: A(N,N), TOL, Q0(N)
    real*8, intent(out) :: AUTOVALOR

    ! Variables propias
    real*8 :: Q(N), Q_ANTERIOR(N), norma, resto(N), AUTOVECTOR(N)
    integer :: i, maxiter

end subroutine auto_potencia_inversa