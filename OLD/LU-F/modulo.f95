module linearalgebra
contains

  subroutine ordenacion(A, d1, d2)
    integer, intent(in) :: d1, d2
    real(8), intent(in) :: A(d1, d2)

    integer :: i

    do i = 1, d1
      write (*, *) A(i, :)
    enddo

  end subroutine ordenacion

  subroutine sustitucion(AB, d1, d2, X)
    integer, intent(in) :: d1, d2
    real(8), intent(in) :: AB(d1, d2)
    real(8), intent(out) :: X(d1)

    integer :: i, j
    real(8) :: h

    do i = d1, 1, -1
      h = AB(i, d2)
      do j = i + 1, d1
        h = h - AB(i, j)*X(j)
      enddo
      X(i) = h/AB(i, i)
    enddo

  end subroutine sustitucion

  subroutine LOWER(A, d1, L, U)
    integer, intent(in) :: d1
    real(8), intent(in) :: A(d1, d1)
    real(8), intent(out) :: L(d1, d1), U(d1, d1)

    integer :: i, j, k
    real(8) :: h, m

    L = 0.d0
    U = 0.d0

    call ordenacion(A, d1, d1)

    L(1, 1) = A(1, 1)

    U(1, 1) = 1.d0

!call ordenacion(U,d1,d1)

    do k = 1, d1 - 1
      U(1, k + 1) = A(1, k + 1)/L(1, 1) !1

      do i = 2, k !2
        h = 0.d0
        do j = 1, i - 1
          h = h + (L(i, j)*U(j, k + 1))
        enddo
        U(i, k + 1) = (A(i, k + 1) - h)/L(i, i)
      enddo

      L(k + 1, 1) = A(k + 1, 1) !3

      do i = 2, k !4
        h = 0.d0
        do j = 1, i - 1
          h = h + (U(j, i)*L(k + 1, j))
        enddo
        L(k + 1, i) = A(k + 1, i) - h
      enddo

      U(k + 1, k + 1) = 1.d0 !5

      m = 0.d0
      do i = 1, k
        m = m + (L(k + 1, i)*U(i, k + 1))
      enddo
      L(k + 1, k + 1) = A(k + 1, k + 1) - m

    enddo

  end subroutine LOWER

  subroutine rellenoA(A, d, B)
    integer :: d
    real(8), intent(inout) :: A(d, d), B(d)

    integer :: i, j, k
    real(8) :: C(d)
    real(8) ::k2

    A(1, 1) = 1.d0
    A(1, 2) = 2.d0
    A(2, 1) = 3.d0
    A(2, 2) = 4.d0
    B(1) = 6.d0
    B(2) = 7.d0
  end subroutine rellenoA

end module linearalgebra
