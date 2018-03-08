subroutine write_A(A,N)
    implicit none
    integer, intent(in) :: N
    real*8, intent(in) :: A(N,N)
    integer :: i

    do i = 1, N
        write(*,*) A(i,:)
    end do
end subroutine write_A

subroutine write_AB(A,B,N)
    implicit none
    integer, intent(in) :: N
    real*8, intent(in) :: A(N,N), B(N)
    integer :: i

    do i = 1, N
        write(*,*) A(i,:), "|", B(i)
    end do
end subroutine write_AB

subroutine write_A_int(A,N)
    implicit none
    integer, intent(in) :: N
    integer, intent(in) :: A(N,N)
    integer :: i
    
    do i = 1, N
        write(*,*) A(i,:)
    end do
end subroutine write_A_int

subroutine pedir_matrix(A,N)
    implicit none
    integer, intent(in) :: N
    real*8, intent(out) :: A(N,N)
    integer :: i, j
    do i = 1, N
        do j = 1, N
            write(*,fmt='(a4,1x,i0,1x,a7,1x,i0)') "Fila", i, "Columna", j
            read(*,*) A(i,j)
        end do
    end do
end subroutine pedir_matrix

subroutine pedir_vector(B,N)
    implicit none
    integer, intent(in) :: N
    real*8, intent(out) :: B(N)
    integer :: i
    do i = 1, N
        write(*,fmt='(a10,1x,i0)') "Componente", i
        read(*,*) B(i)
    end do
end subroutine pedir_vector