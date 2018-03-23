program gauss
  use linearalgebra
  implicit none
  real(8), allocatable :: A(:, :), X(:), B(:), L(:, :), U(:, :), test(:, :)
  integer :: d, d1

  write (*, *) 'Tamaño de matriz'
  read (*, *) d
  d1 = d + 1

  allocate (A(d, d))
  allocate (X(d))
  allocate (B(d))
  allocate (L(d, d))
  allocate (U(d, d))
  allocate (test(d, d))

  call rellenoA(A, d, B)

!AB(1:d,1:d)=A
!AB(1:d,d1)=B

  write (*, *) 'A'
  call ordenacion(A, d, d)
!write(*,*) 'AB'
!call ordenacion(AB, d, d1)
  write (*, *) 'B'
  write (*, *) B

  call LOWER(A, d, L, U)

  write (*, *) 'L es'
  call ordenacion(L, d, d)
  write (*, *) 'U es'
  call ordenacion(U, d, d)

  write (*, *) A
  write (*, *) matmul(L, U)

  test = MATMUL(L, U)
  write (*, *) 'test'
  call ordenacion(test, d, d)
!write(*,*) test
  write (*, *) 'L(8,8)', L(8, 8)
  write (*, *) 'U(7,8)', U(7, 8)

  write (*, *) 'X(8)', X(8)

end program gauss
