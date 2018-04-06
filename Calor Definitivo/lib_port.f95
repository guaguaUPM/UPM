module lib_port
contains

  subroutine matrizcontorno(MATRIZ, PARTICIONES, X1, X2)
    ! SUBRUTINA QUE CALCULA UNA MATRIZ DE CONTORNO GENERICA DADO EL
    ! NUMERO DE PARTICIONES EN LOS QUE SE QUIERA DISCRETIZAR NUESTRA VARIABLE
    ! Y EL TAMAÑO DEL INCREMENTO (DEDUCIDO A TRAVES DEL PUNTO INICIAL Y FINAL)
    implicit none

    ! ARGUMENTOS
    real*8, intent(in)  :: X1, X2
    integer, intent(in) :: PARTICIONES
    real*8, intent(out) :: MATRIZ(PARTICIONES, PARTICIONES)

    ! VARIABLES LOCALES
    real*8              :: incremento
    integer             :: i, j

    incremento = abs((X2 - X1)/(PARTICIONES*1.d0))
    MATRIZ = 0.d0
    MATRIZ(1, 2) = 1/(incremento*1.d0)
    MATRIZ(1, 1) = -1/(incremento*1.d0)
    MATRIZ(PARTICIONES, PARTICIONES) = 1/(incremento*1.d0)
    MATRIZ(PARTICIONES, PARTICIONES - 1) = -1/(incremento*1.d0)
    do i = 2, PARTICIONES - 1
      do j = 2, PARTICIONES - 1
        MATRIZ(i, i + 1) = 1/(2.d0*incremento)
        MATRIZ(i, i - 1) = -1/(2.d0*incremento)
      enddo
    enddo
  end subroutine

  subroutine convergencia(T, converge, N)
    implicit none
    integer, intent(in) :: N
    real*8, intent(in)  :: T(N, N)
    logical, intent(out):: converge

    real*8              :: autovalor, q0(N)

    q0 = 1.0d0
    call auto_potencia_iter(T, autovalor, 60, q0, N)
    converge = .false.
    if (abs(autovalor) < 1.d0) converge = .true.
  end subroutine convergencia

  subroutine auto_potencia_iter(A, AUTOVALOR, MAXITER, Q0, N)
    !CALCULA EL AUTOVALOR MAS GRANDE EN VALOR ABSOLUTO
    implicit none

    ! Variables de entrada/salida
    integer, intent(in) :: N, MAXITER
    real*8, intent(in)  :: A(N, N), Q0(N) !SELECCIONAR UN q0 (1.d0 por lo general)
    real*8, intent(out) :: AUTOVALOR

    ! Variables propias
    real*8 :: Q(N), Q_ANTERIOR(N), AUTOVECTOR(N), norma
    integer :: i, iter

    iter = MAXITER

    Q = Q0
    call norma2(norma, Q, N)
    if (norma /= 1.d0) then
      Q = Q/norma
    endif

    !   CALCULO DEL AUTOVECTOR
    do i = 1, maxiter

      Q_ANTERIOR = Q

      Q = matmul(A, Q)

      call norma2(norma, Q, N)
      Q = Q/norma

    enddo

    AUTOVECTOR = Q

    !   CALCULO DEL AUTOVALOR ASOCIADO (coef. de Rayleigh)
    AUTOVALOR = DOT_PRODUCT(AUTOVECTOR, matmul(A, AUTOVECTOR))/(DOT_PRODUCT(AUTOVECTOR, AUTOVECTOR)*1.d0)
  end subroutine auto_potencia_iter

  subroutine norma2(norma, vector, n)
    implicit none
    integer, intent(in)  :: n
    real(8), intent(in)  :: vector(n)
    real(8), intent(out) :: norma
    integer :: i

    norma = 0.d0
    do i = 1, n
      norma = norma + vector(i)**2
    end do
    norma = sqrt(norma)
  end subroutine norma2

  subroutine matrizT_gauss_seidel(A, T, N)
    ! Argumentos de la subrutina
    integer, intent(in) :: N
    real(8), intent(in) :: A(N, N)
    real(8), intent(out):: T(N, N)

    ! Variables locales
    real(8), allocatable :: L(:, :)
    real(8), allocatable :: D(:, :)
    real(8), allocatable :: U(:, :)
    real(8), allocatable :: P1(:, :)
    real(8), allocatable :: P2(:, :)
    integer :: j, k

    allocate (L(n, n))
    allocate (D(n, n))
    allocate (U(n, n))
    allocate (P1(n, n))
    allocate (P2(n, n))

    ! Establecemos las matrices L, D y U para el algoritmo de la iteración
    do j = 1, n
      do k = 1, n
        if (k > j) then
          U(j, k) = A(j, k)
        else if (k == j) then
          D(j, k) = A(j, k)
        else
          L(j, k) = A(j, k)
        end if
      end do
    end do

    ! Calculamos T segun la expresion matematica
    P2 = U
    call inversa(D + L, P1, N)
    P1 = (-1.d0)*P1

    T = MATMUL(P1, P2)

  end subroutine matrizT_gauss_seidel

  subroutine inversa(matriz, c, n)
    !============================================================
    ! Inverse matrix
    ! Method: Based on Doolittle LU factorization for Ax=b
    ! Alex G. December 2009
    !-----------------------------------------------------------
    ! input ...
    ! matrix(n,n) - array of coefficients for matrix A
    ! n           - dimension
    ! output ...
    ! c(n,n)      - inverse matrix of A
    !===========================================================
    implicit none
    integer, intent(in)     :: n
    real*8, intent(in)      :: matriz(n, n)
    real*8, intent(out)     :: c(n, n)
    real*8                  :: a(n, n), L(n, n), U(n, n), b(n), d(n), x(n)
    real*8 :: coeff
    integer :: i, j, k

    ! step 0: initialization for matrices L and U and b
    ! Fortran 90/95 aloows such operations on matrices
    a = matriz
    L = 0.d0
    U = 0.d0
    b = 0.d0

    ! step 1: forward elimination
    do k = 1, n - 1
      do i = k + 1, n
        coeff = a(i, k)/a(k, k)
        L(i, k) = coeff
        do j = k + 1, n
          a(i, j) = a(i, j) - coeff*a(k, j)
        end do
      end do
    end do

    ! Step 2: prepare L and U matrices
    ! L matrix is a matrix of the elimination coefficient
    ! + the diagonal elements are 1.0
    do i = 1, n
      L(i, i) = 1.0
    end do
    ! U matrix is the upper triangular part of A
    do j = 1, n
      do i = 1, j
        U(i, j) = a(i, j)
      end do
    end do

    ! Step 3: compute columns of the inverse matrix C
    do k = 1, n
      b(k) = 1.0
      d(1) = b(1)
      ! Step 3a: Solve Ld=b using the forward substitution
      do i = 2, n
        d(i) = b(i)
        do j = 1, i - 1
          d(i) = d(i) - L(i, j)*d(j)
        end do
      end do
      ! Step 3b: Solve Ux=d using the back substitution
      x(n) = d(n)/U(n, n)
      do i = n - 1, 1, -1
        x(i) = d(i)
        do j = n, i + 1, -1
          x(i) = x(i) - U(i, j)*x(j)
        end do
        x(i) = x(i)/u(i, i)
      end do
      ! Step 3c: fill the solutions x(n) into column k of C
      do i = 1, n
        c(i, k) = x(i)
      end do
      b(k) = 0.0
    end do
  end subroutine inversa

  subroutine resolver_jacobi_tol(A, Xfinal, b, tol, N)

    ! Argumentos de la subrutina
    real(8), intent(in) :: A(N, N) !
    real(8), intent(in) :: b(N) !
    real(8), intent(out):: Xfinal(n) !
    real(8), intent(in) :: tol !
    integer, intent(in) :: N ! Dimensión del problema A(n,n) b(n) X(n)

    ! Variables locales
    real(8), allocatable :: x0(:)
    real(8), allocatable :: x(:)
    integer :: iter, i, j, maxiter
    real(8) :: sum1, normaX_X0, normaX

    allocate (x0(N))
    allocate (x(N))

    !Primera semilla (primer valor establecido de x para iniciar la iteración)
    x0 = 0.d0
    x = 0.0d0
    maxiter = 999999

    do iter = 1, maxiter
      do i = 1, n
        sum1 = 0
        do j = 1, n
          if (j /= i) then
            sum1 = sum1 + A(i, j)*x0(j)
          endif
        enddo
        x(i) = (1/A(i, i))*(B(i) - sum1)
      enddo

      call norma2(normaX_X0, X - x0, n)
      call norma2(normaX, x, n)

      if (normaX_X0/normaX <= tol) then
        Xfinal = x
        return
      else
        x0 = x
      end if
    enddo
  end subroutine resolver_jacobi_tol

end module lib_port
