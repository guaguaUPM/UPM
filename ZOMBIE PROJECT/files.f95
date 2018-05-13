module files
contains
    subroutine create_and_clean
        implicit none

        ! Con status='replace' se crea el archivo si no existia, o se restea en el caso contrario.

        open(21, file='T_S.dat', status='replace')
        close(21)

        open(22, file='T_Z.dat', status='replace')
        close(22)

        open(23, file='T_R.dat', status='replace')
        close(23)
    end subroutine create_and_clean

    subroutine leer_parametros(PARAM)
        implicit none
        real*8, intent(inout) :: PARAM(5)
        character (len=200)   :: line
        integer :: info, i

        open(unit=69,file='PARAM.conf',status='old',action='read',iostat=info)
        if(info==0) then
            do i=1,5
                30 CONTINUE
                read(69,*) line
                ! write(*,*) line(1:1) ! DEBUG
                if(line(1:1) .EQ. '!') goto 30
                read(line,*) PARAM(i)
                ! write(*,*) PARAM(i) ! DEBUG
            end do
        else 
            write(*,*) "Ha habido un error en la lectura del archivo"
            write(*,*) "Si se ha borrado o esta corrupto, reestablecer el original"
        end if
        close(69)
    end subroutine leer_parametros
end module files