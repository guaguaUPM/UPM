!REQUISITOS: A SEA MATRIZ CUADRADA, TODOS LOS AUTOVALORES DE A SEAN REALES 
!   Y QUE ESTOS SEAN UN CONJUNTO ORDENADO

subroutine auto_potencia (A, AUTOVALOR, TOL, Q0, N)
    !CALCULA EL AUTOVALOR MAS GRANDE EN VALOR ABSOLUTO
    implicit none

    ! Variables de entrada/salida
    integer, intent(in) :: N
    real*8, intent(in)  :: A(N,N), TOL, Q0(N)   !SELECCIONAR UN q0 (1.d0 por lo general)
    real*8, intent(out) :: AUTOVALOR

    ! Variables propias
    real*8 :: Q(N), Q_ANTERIOR(N), AUTOVECTOR(N), norma, resto, sumaQ, sumaQ_ant
    integer :: i, j, maxiter

    maxiter = 999999

    Q = Q0
    call norma2(norma,Q,N)
    if (norma /= 1.d0) then
        Q = Q/norma
    endif
    write(*,*) Q
    read(*,*)

    !   CALCULO DEL AUTOVECTOR
    do i = 1, maxiter

        sumaQ = 0.d0
        sumaQ_ant = 0.d0

        Q_ANTERIOR = Q

        Q = matmul(A,Q)

        call norma2(norma,Q,N)
        Q = Q/norma
        write(*,*) Q
        read(*,*)
        
        do j= 1, N
            sumaQ = sumaQ + abs(Q(j))
            sumaQ_ant = sumaQ_ant + abs(Q_ANTERIOR(j))
        enddo
        resto = sumaQ - sumaQ_ant
        if( abs(resto) <tol) exit

    enddo
    
    AUTOVECTOR = Q

    !   CALCULO DEL AUTOVALOR ASOCIADO (coef. de Rayleigh)
    AUTOVALOR = DOT_PRODUCT(AUTOVECTOR, matmul(A, AUTOVECTOR)) / DOT_PRODUCT(AUTOVECTOR, AUTOVECTOR)

end subroutine auto_potencia

subroutine auto_potencia_inversa (A, AUTOVALOR, TOL, Q0, N)
    !CALCULA EL AUTOVALOR MAS PEQUEÑO EN VALOR ABSOLUTO
    implicit none

    ! Variables de entrada/salida
    integer, intent(in) :: N
    real*8, intent(in)  :: A(N,N), TOL, Q0(N)    !SELECCIONAR UN q0 (1.d0 por lo general)
    real*8, intent(out) :: AUTOVALOR

    ! Variables propias
    real*8 :: Q(N), Q_ANTERIOR(N), norma, AUTOVECTOR(N)
    integer :: i, maxiter

    maxiter = 60

    Q = Q0
    call norma2(norma,Q,N)
    if (norma /= 1.d0) then
        Q = Q/norma
    endif

    !   CALCULO DEL AUTOVECTOR
    do i = 1, maxiter

        Q_ANTERIOR = Q

        call resolver_gauss (A, Q_ANTERIOR, Q, N)

        call norma2(norma,Q,N)
        Q = Q/norma

    enddo
    
    AUTOVECTOR = Q

    !   CALCULO DEL AUTOVALOR ASOCIADO (coef. de Rayleigh)
    AUTOVALOR = DOT_PRODUCT(AUTOVECTOR, matmul(A, AUTOVECTOR)) / DOT_PRODUCT(AUTOVECTOR, AUTOVECTOR)

end subroutine auto_potencia_inversa