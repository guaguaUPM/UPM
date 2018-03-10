! Para usar modulos de este archivo descomentar los argumentos de LAPACK en proyecto en cuestion

subroutine resolver_LAPACK(A,B,X,N)
    implicit none
    integer, intent(in)  :: N
    real*8, intent(in)   :: A(N,N), B(N)
    real*8, intent(out)  :: X(N)
    integer              :: IPIV(N,N), info
    X=B
    call DGESV(N,1,A,N,IPIV,X,N,info)
    if(info < 0) then
        write(*,*) "El argumento", -info, "es incorrecto"
    else if (info > 0) then
        write(*,*) "La factorización ha fallado"
    end if
end subroutine resolver_LAPACK