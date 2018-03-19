program autovalores
    implicit none
    real*8, allocatable :: A(:,:), B(:), tol, sol(:)
    integer :: N

    ! Se piden los datos de la matriz y q0 al usuario
    write(*,*) "¿Que N desea?"
    read(*,*) N
    allocate(A(N,N))
    allocate(B(N))
    allocate(sol(N))
    call pedir_matrix(A,N)
    call pedir_vector(B,N)
    call write_AB(A,B,N)
    ! -----------------------------------------

    tol = 0.0005d0
    call resolver_jacobi_tol(A, sol, B, tol, N)
    write(*,*) "Jacobi:", sol
    call resolver_gauss_seidel_tol(A, sol, B, tol, N)
    write(*,*) "Gauss-Seidel:", sol
    call resolver_gauss(A, B, sol, N)
    write(*,*) "Gauss", sol
end program autovalores