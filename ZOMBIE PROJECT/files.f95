module files
contains
    subroutine create_and_clean
        implicit none

        ! Con status='replace' se crea el archivo si no existia, o se restea en el caso contrario.
        open(24, file='T_SZR.dat', status='replace')
        close(24)

    end subroutine create_and_clean

    subroutine leer_parametros(PARAM,K)
        implicit none
        real*8, intent(inout) :: PARAM(5), K
        character (len=200) :: line
        integer :: info, i

        open(unit=69,file='PARAM.conf',status='old',action='read',iostat=info)
        if(info==0) then
            do i=1,5
                30 CONTINUE
                read(69,*) line
                ! write(*,*) line(1:1) ! DEBUG
                if(line(1:1) .EQ. '#') goto 30
                read(line,*) PARAM(i)
                ! write(*,*) PARAM(i) ! DEBUG
            end do
            40 CONTINUE
            read(69,*) line
            if(line(1:1) .EQ. '#') goto 40
            read(line,*) K
        else 
            write(*,*) "Ha habido un error en la lectura del archivo"
            write(*,*) "Si se ha borrado o esta corrupto, reestablecer el original"
        end if
        close(69)
    end subroutine leer_parametros
    
    subroutine leer_valores_iniciales(S,Z,R)
        implicit none
        real*8, intent(out) :: S, Z, R
        character(len=200)  :: line
        integer :: info, i

        open(unit=42,file='INIT.conf',status='old',action='read',iostat=info)
        if (info == 0) then
            51 CONTINUE
            read(42,*) line
            if(line(1:1) .EQ. '#') goto 51
            read(line,*) S

            52 CONTINUE
            read(42,*) line
            if(line(1:1) .EQ. '#') goto 52
            read(line,*) Z

            53 CONTINUE
            read(42,*) line
            if(line(1:1) .EQ. '#') goto 53
            read(line,*) R
        else 
            write(*,*) "Ha habido un error en la lectura del archivo"
            write(*,*) "Si se ha borrado o esta corrupto, reestablecer el original"
        end if
        close(42)
    end subroutine leer_valores_iniciales
end module files