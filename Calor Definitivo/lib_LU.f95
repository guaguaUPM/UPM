module lib_LU
contains

  subroutine resolver_LU(A, B, X, N)
    implicit none
    integer, intent(in) :: N
    real*8, intent(in) :: A(N, N), B(N)
    real*8, intent(out) :: X(N)

    real*8 :: L(N, N), U(N, N), Y(N), B_NORMALIZADA(N)
    integer :: i

    call LU_factorizar(A, L, U, N)

    B_NORMALIZADA = B
    do i = 1, N
      B_NORMALIZADA(i) = B_NORMALIZADA(i)/L(i, i)
      L(i, :) = L(i, :)/L(i, i)
    end do
    call gauss_sustituir(L, B_NORMALIZADA, Y, N, .FALSE.)
    call gauss_sustituir(U, Y, X, N, .TRUE.)
  end subroutine resolver_LU

  subroutine LU_factorizar(A, L, U, N)
    implicit none
    real(8), intent(in)  :: A(n, n)
    integer, intent(in)  :: n
    real(8), intent(out) :: L(N, N), U(N, N)

    integer :: i, j, k
    real(8) :: sum

    ! Aplicamos el algoritmo matemático asociado a la factorización LU
    ! (Que nadie sabe lo que hace pero funciona)

    L = 0.d0
    U = 0.d0
    L(1, 1) = A(1, 1)
    U(1, 1) = 1
    do k = 1, n - 1
      U(1, k + 1) = A(1, k + 1)/L(1, 1)
      do i = 2, k
        sum = 0
        do j = 1, i - 1
          sum = L(i, j)*U(j, k + 1) + sum
        end do
        U(i, k + 1) = (A(i, k + 1) - sum)/L(i, i)
      end do
      L(k + 1, 1) = A(k + 1, 1)
      do i = 2, k
        sum = 0
        do j = 1, i - 1
          sum = U(j, i)*L(k + 1, j) + sum
        end do
        L(k + 1, i) = (A(k + 1, i) - sum)
      end do
      U(k + 1, k + 1) = 1
      sum = 0
      do i = 1, k
        sum = L(k + 1, i)*U(i, k + 1) + sum
      end do
      L(k + 1, k + 1) = A(k + 1, k + 1) - sum
    end do
  end subroutine LU_factorizar

  subroutine gauss_sustituir(A, B, RESULTADO, TAMANO, MODO)
    ! Se sustituyen los valores del sistema AX=B

    ! CON CEROS EN LA DIAGONAL

    ! Si MODO es .TRUE., se interpreta la matriz de entrada como triangular superior:
    ! 1 3 4 | 0.1
    ! 0 1 5 | 0.2
    ! 0 0 1 | 0.3
    ! Si MODO es .FALSE., se interpreta como triangular inferior:
    ! 1 0 0 | 0.1
    ! 3 1 0 | 0.2
    ! 4 5 1 | 0.3

    implicit none
    integer, intent(in)  :: TAMANO
    real*8, intent(in)   :: A(TAMANO, TAMANO), B(TAMANO)
    real*8, intent(out)  :: RESULTADO(TAMANO)
    logical, intent(in)  :: MODO
    integer              :: i, j

    if (modo) then
      do i = TAMANO, 1, -1
        RESULTADO(i) = B(i)
        do j = i + 1, TAMANO
          RESULTADO(i) = RESULTADO(i) - A(i, j)*RESULTADO(j)
        end do
      end do
    else
      do i = 1, TAMANO
        RESULTADO(i) = B(i)
        do j = 1, i - 1
          RESULTADO(i) = RESULTADO(i) - A(i, j)*RESULTADO(j)
        end do
      end do
    end if
  end subroutine gauss_sustituir

end module lib_LU
