program autovalores
    implicit none
    real*8, allocatable :: A(:,:), Ai(:,:)
    integer :: N

    ! Se piden los datos de la matriz y q0 al usuario
    write(*,*) "¿Que tamano de matriz desea?"
    read(*,*) N
    allocate(A(N,N))
    allocate(Ai(N,N))
    call pedir_matrix(A,N)
    call write_A(A,N)
    ! -----------------------------------------

    call inversa(A,Ai,N)
    call write_A(Ai,N)

end program autovalores