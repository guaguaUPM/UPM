program test
use alg_general

real*8 :: A(3,3), B(3), X(3)
real*8 :: A1(3,3), B1(3)
integer :: i, j
integer :: PIVOT(3,3)

A(2,1) = 1.d0
A(2,2) = 0.D0
A(2,3) = 0.D0
A(1,1) = 3.d0
A(1,2) = 1.d0
A(1,3) = 0.d0
A(3,1) = 4.d0
A(3,2) = 5.d0
A(3,3) = 1.d0
B(1) = 0.1d0
B(2) = 0.2d0
B(3) = 0.3d0

do i = 1, 3
    write(*,*) A(i,:), "|", B(i)
end do

write(*,*)

call pivotar (A,A1,B,B1, PIVOT, 3)

do i = 1, 3
    write(*,*) A1(i,:), "|", B1(i), "|",PIVOT(i,:)
end do

    write(*,*)
A1 = matmul(PIVOT,A1)
do i = 1, 3
    write(*,*) A1(i,:)
end do

end program test