program autovalores
    implicit none
    real*8, allocatable :: A(:,:), q0(:)
    real*8 :: autovalor_max, autovalor_min, tol
    integer :: N

    ! Se piden los datos de la matriz y q0 al usuario
    write(*,*) "¿Que tamano de matriz desea?"
    read(*,*) N
    allocate(A(N,N))
    allocate(q0(N))
    q0 = 1.d0
    call pedir_matrix(A,N)
    call write_A(A,N)
    ! -----------------------------------------

    tol = 0.000005
    call auto_potencia_iter(A, autovalor_max, 100000, q0, N)
    call auto_potencia_inversa_iter(A, autovalor_min, 1000, q0, N)

    write(*,*) "Autovalor Maximo:", autovalor_max
    write(*,*) "Autovalor Minimo:", autovalor_min

end program autovalores