program autovalores
    implicit none
    real*8, allocatable :: A(:,:), Valores(:),work(:),basura(:)
    real*8 :: autovalor, tol
    integer :: N,info,LWORK,LDA
    character*1 :: jobz,UPLO,ILO,IHI
    
    ! Se piden los datos de la matriz al usuario
    write(*,*) "¿Que tamano de matriz desea?"
    read(*,*) N
    allocate(A(N,N))
    allocate(Valores(N))
   
    Write(*,*) "Escriba una matriz !Simetrica!"
    call pedir_matrix(A,N)
    call write_A(A,N)
    !write (*,*) "Escribe N para calcular los autovectores, luego escribe U o L"
    !read(*,*) jobz
    !read(*,*) UPLO
    ! -----------------------------------------
    LWORK = 10000
    jobz = "B"
    UPLO = "U"
    LDA = N
   call DGBAL(jobz,n,A,lda,ILO,IHI,valores,info)

    if (info == 0) then
        write(*,*) "De puta madre"
    else
        write(*,*) "Oops"
    endif

end program autovalores