program main
  implicit none
  call timestamp ( )
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'LAPACK_PRB'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  Test the LAPACK library.'

  call test01 ( )

  !
  !  Terminate.
  !
write ( *, '(a)' ) ' '
write ( *, '(a)' ) 'LAPACK_PRB'
write ( *, '(a)' ) '  Normal end of execution.'
write ( *, '(a)' ) ' '
call timestamp ( )

stop
end program main

subroutine test01 ( )

  !*****************************************************************************80
  !
  !! TEST01 tests DGBTRF and DGBTRS.
  !
  !  Discussion:
  !
  !    The problem is just an enlarged version of the
  !    problem for n = 5, which is:
  !
  !    Matrix A is ( 2 -1  0  0  0)    right hand side b is  (1)
  !                (-1  2 -1  0  0)                          (0)
  !                ( 0 -1  2 -1  0)                          (0)
  !                ( 0  0 -1  2 -1)                          (0)
  !                ( 0  0  0 -1  2)                          (1)
  !
  !
  !    Solution is   (1)
  !                  (1)
  !                  (1)
  !                  (1)
  !                  (1)
  !
  !  Licensing:
  !
  !    This code is distributed under the GNU LGPL license.
  !
  !  Modified:
  !
  !    12 September 2006
  !
  !  Author:
  !
  !    John Burkardt
  !
    implicit none
  
    integer ( kind = 4 ), parameter :: n = 25
    integer ( kind = 4 ), parameter :: ml = 1
    integer ( kind = 4 ), parameter :: mu = 1
  
    integer ( kind = 4 ), parameter :: lda = 2 * ml + mu + 1
  
    real ( kind = 8 ) a(lda,n)
    real ( kind = 8 ) b(n)
    integer ( kind = 4 ) info
    integer ( kind = 4 ) ipiv(n)
    integer ( kind = 4 ) m
  
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'TEST01'
    write ( *, '(a)' ) '  For a double precision real matrix (D)'
    write ( *, '(a)' ) '  in general band storage mode (GB):'
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  DGBTRF factors a general band matrix.'
    write ( *, '(a)' ) '  DGBTRS solves a factored system.'
    write ( *, '(a)' ) ' '
  !
  !  Assign values to matrix A and right hand side b.
  !
    b(1) = 1.0D+00
    b(2:n-1) = 0.0D+00
    b(n) = 1.0D+00
  !
  !  Zero out the matrix.
  !
    a(1:lda,1:n) = 0.0D+00
  
    m = ml + mu + 1
  !
  !  Superdiagonal,
  !  Diagonal,
  !  Subdiagonal.
  !
    a(m-1,2:n) = -1.0D+00
    a(m,1:n) = 2.0D+00
    a(m+1,1:n-1) = -1.0D+00
  !
  !  Factor the matrix.
  !
    write ( *, '(a)' ) ' '
    write ( *, '(a,i8)' ) '  Bandwidth is ', m
    write ( *, '(a)' ) ' '
  
    call dgbtrf ( n, n, ml, mu, a, lda, ipiv, info )
  
    if ( info /= 0 ) then
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST01'
      write ( *, '(a,i8)' ) '  Factorization failed, INFO = ', info
      return
    end if
  !
  !  Solve the linear system.
  !
    call dgbtrs ( 'n', n, ml, mu, 1, a, lda, ipiv, b, n, info )
  
    if ( info /= 0 ) then
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST01'
      write ( *, '(a,i8)' ) '  Solution failed, INFO = ', info
      return
    end if
  
    call r8vec_print_some ( n, b, 1, 5, '  Partial solution (all should be 1)' )
  
    return
end subroutine test01