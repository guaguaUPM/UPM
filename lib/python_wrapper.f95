subroutine represtarR1_R1(FUNCION,INICIO,FIN,PARTICIONES)
    implicit none
    real*8, intent(in)  :: INICIO, FIN
    integer, intent(in) :: PARTICIONES
    interface
        function FUNCION(X)
            real*8 :: X
            real*8 :: FUNCION
        end function
    end interface
    integer :: info, i
    real*8  :: incremento, abcisa

    open(unit=10,file='valores.dat',status='unknown',action='write',iostat=info)
    incremento = (FIN-INICIO)/(PARTICIONES*1.d0)
    do i=0, PARTICIONES
        abcisa = INICIO + i * incremento
        write(10,*) abcisa,FUNCION(abcisa)
    end do
    close(10)
    call SYSTEM("python plot.py")
end subroutine represtarR1_R1