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
    call SYSTEM("python plot11.py")
end subroutine represtarR1_R1

subroutine represtarR2_R1(FUNCION,INICIO_X,FIN_X,INICIO_Y,FIN_Y,PARTICIONES_X, PARTICIONES_Y)
    implicit none
    real*8, intent(in)  :: INICIO_X,FIN_X,INICIO_Y,FIN_Y
    integer, intent(in) :: PARTICIONES_X, PARTICIONES_Y
    interface
        function FUNCION(X,Y)
            real*8 :: X,Y
            real*8 :: FUNCION
        end function
    end interface
    integer :: info, i,j
    real*8  :: incremento_x, incremento_y, posX, posY
    
    open(unit=10,file='valores.dat',status='unknown',action='write',iostat=info)

    incremento_x = (FIN_X-INICIO_X)/(PARTICIONES_X*1.d0)
    incremento_Y = (FIN_Y-INICIO_Y)/(PARTICIONES_Y*1.d0)

    do i=0, PARTICIONES_X
        posX = INICIO_X + i*incremento_x
        do j=0, PARTICIONES_Y
            posY = INICIO_Y + j*incremento_y
            write(10,*) int(posX), int(posY), int(FUNCION(posX,posY))
        end do
    end do
    close(10)
    call SYSTEM("python plot21.py")
end subroutine represtarR2_R1

subroutine represtarR1_R1_tangente(FUNCION,INICIO,FIN,PARTICIONES,X0,M)
    implicit none
    real*8, intent(in)  :: INICIO, FIN, X0 ,M
    integer, intent(in) :: PARTICIONES
    interface
        function FUNCION(X)
            real*8 :: X
            real*8 :: FUNCION
        end function
    end interface
    integer :: info, i
    real*8  :: incremento, abcisa, y0

    open(unit=10,file='valores.dat',status='unknown',action='write',iostat=info)
    incremento = (FIN-INICIO)/(PARTICIONES*1.d0)
    do i=0, PARTICIONES
        abcisa = INICIO + i * incremento
        write(10,*) abcisa,FUNCION(abcisa)
    end do
    close(10)

    open(unit=11,file='derivada.dat',status='unknown',action='write',iostat=info)
    y0=FUNCION(X0)

    abcisa = x0-(PARTICIONES/2)*incremento
    write(11,*) abcisa, M*(abcisa-x0) + y0
    abcisa = x0+(PARTICIONES/2)*incremento
    write(11,*) abcisa, M*(abcisa-x0) + y0
    close(11)

    call SYSTEM("python plot11_derivada.py")
end subroutine represtarR1_R1_tangente