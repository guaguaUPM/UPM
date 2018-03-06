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