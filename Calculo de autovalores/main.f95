program autovalores
    implicit none
    real*8, allocatable :: A(:,:)
    real*8 :: autovalor, tol
    integer :: N

    
    ! Se piden los datos de la matriz al usuario
    write(*,*) "¿Que tamano de matriz desea?"
    read(*,*) N
    allocate(A(N,N))
    call pedir_matrix(A,N)
    call write_A(A,N)
    ! -----------------------------------------

    call auto_potencia(A,autovalor,tol,N)

end program autovalores