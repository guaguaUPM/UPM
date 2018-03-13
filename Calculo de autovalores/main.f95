program autovalores
    implicit none
    real*8, allocatable :: A(:,:)
    integer :: N

    
    ! Se piden los datos de la matriz al usuario
    write(*,*) "¿Que tamano de matriz desea?"
    read(*,*) N
    allocate(A(N,N))
    call pedir_matrix(A,N)
    call write_A(A,N)
    ! -----------------------------------------

end program autovalores