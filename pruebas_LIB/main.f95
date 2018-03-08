program main
implicit none
real*8, allocatable :: A(:,:), B(:), X(:)
integer :: N

! ----------------------------
write(*,*) "¿Que dimension desea?"
read(*,*) N
allocate(A(N,N))
allocate(B(N))
allocate(X,mold=B)

call pedir_matrix(A,N)
call pedir_vector(B,N)

call write_AB(A,B, N)
write(*,*)
! ------------------------------


call resolver_LU(A,B,X,N)
write(*,*) X

end program main