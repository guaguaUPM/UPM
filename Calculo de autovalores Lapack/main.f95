program autovalores
    implicit none
    real*8, allocatable :: A(:,:), Valores(:),work(:),basura(:)
    real*8 :: autovalor, tol
    integer :: N,info,LWORK,LDA
    character*1 :: jobz,UPLO
    
    ! Se piden los datos de la matriz al usuario
    write(*,*) "¿Que tamano de matriz desea?"
    read(*,*) N
    allocate(A(N,N))
    allocate(Valores(N))
    allocate(work(N))
    allocate(basura(n-1))
    Write(*,*) "Escriba una matriz"
    call pedir_matrix(A,N)
    call write_A(A,N)
    !write (*,*) "Escribe N para calcular los autovectores, luego escribe U o L"
    !read(*,*) jobz
    !read(*,*) UPLO
    ! -----------------------------------------
    LWORK = 10000
    jobz = "N"
    UPLO = "U"
    LDA = N
   
    call SGEHRD(n,1,N,A,n,basura,work,Lwork,info)
    write(*,*) info
    call DSYEV(jobz,UPLO,n,A,n,Valores,work,LWORK,info) 
    Lwork = work(1)
    call DSYEV(jobz,UPLO,n,A,LDA,Valores,work,LWORK,info) 

    Write(*,*) "Los autovalores son:",Valores
    write(*,*) work(1)

    if (info == 0) then
        write(*,*) "De puta madre"
    else
        write(*,*) "Oops"
    endif

end program autovalores