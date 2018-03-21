program main
implicit none
real*8, allocatable :: A(:,:), B(:), X(:)
integer :: N

write(*,*) "Resolucion de sistemas con todos los metodos"
! ----------------------------
write(*,*) "¿Que dimension desea?"
read(*,*) N
allocate(A(N,N))
allocate(B(N))
allocate(X,mold=B)

call pedir_matrix(A,N)
call pedir_vector(B,N) !holi
call write_AB(A,B, N)
write(*,*)
! ------------------------------

call resolver_gauss(A,B,X,N)
write(*,*) "Resolucion por Gauss:"
write(*,*) X

call resolver_LU(A,B,X,N)
write(*,*) "Resolucion por LU:"
write(*,*) X

call resolver_jacobi_iter (A,B,X,N,10000)
write(*,*) "Resolución por Jacobi, 10000 iteraciones"
write(*,*) X

call resolver_gauss_seidel_iter (A,B,X,N,10000)
write(*,*) "Resolución por Gauss-Seidel, 10000 iteraciones"
write(*,*) X

call resolver_LAPACK(A,B,X,N)
write(*,*) "Resolucion por LAPACK:"
write(*,*) X



end program main