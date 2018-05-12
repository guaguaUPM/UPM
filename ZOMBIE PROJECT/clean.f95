module clean
contains
subroutine create_and_clean
    implicit none

    ! Con status='replace' se crea el archivo si no existia, o se restea en el caso contrario.

    open(11, file='T_S.dat', status='replace')
    close(11)

    open(12, file='T_Z.dat', status='replace')
    close(12)

    open(13, file='T_R.dat', status='replace')
    close(13)
end subroutine create_and_clean
end module clean