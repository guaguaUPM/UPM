program autovalores
    implicit none
    real*8, allocatable :: A(:,:), B(:), T(:,:)
    integer :: N
    logical :: logic

    ! Se piden los datos de la matriz y q0 al usuario
    write(*,*) "¿Que N desea?"
    read(*,*) N
    allocate(A(N,N))
    allocate(B(N))
    allocate(T(N,N))
    call pedir_matrix(A,N)
    call pedir_vector(B,N)
    call write_AB(A,B,N)
    ! -----------------------------------------

    call matrizT_jacobi (A, T, N)
    call convergencia (T, logic, N)
    call write_A_int (T, N)
    write(*,*) logic
    call matrizT_gauss_seidel (A, T, N)
    call convergencia (T, logic, N)
    call write_A_int (T, N)
    write(*,*) logic

end program autovalores