program main
implicit none

real*8, allocatable :: A(:,:), B(:), A_PIVOT(:,:), B_PIVOT(:)
integer, allocatable :: pivote(:,:)
integer :: N, i, j

read(*,*) N
allocate(A(N,N))
allocate(B(N))
allocate(A_PIVOT, mold=A)
allocate(B_PIVOT, mold=B)
allocate(pivote(N,N))

do i=1, N
    B(i) = i - 1
    do j=1, N
        A(i,j) = i + j
    end do
end do

A(2,2) = 0.d0 
call write_AB(A,B, N)


write(*,*)

call pivotar(A,A_PIVOT, B, B_PIVOT, pivote, N)
call write_AB(A_PIVOT, B_PIVOT, N)
write(*,*)
call write_A_int(pivote, N)

end program main