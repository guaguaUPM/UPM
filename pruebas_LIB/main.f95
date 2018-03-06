program main
implicit none

real*8, allocatable :: A(:,:), B(:), permu(:,:), A2(:,:), B2(:)
integer :: i, j, N


! SE GENERA UNA MATRIZ DE PRUEBA --------
write(*,*) "¿Que tamano desea?"
read(*,*) N
allocate(A(N,N))
allocate(B(N))
allocate(permu, mold=A)
allocate(A2, mold=permu)
allocate(B2, mold=B)

do i = 1, N
    B(i) = i-1
    do j=1, N
        A(i,j) = i + j
    end do
end do

do i = 1, N
    write(*,*) A(i,:), "|", B(i)
end do
!---------------------------------------



!call pivotar(A,A2,B,B2, permu, 3)

!do i = 1, 3
!    write(*,*) A2(i,:), "|", B2(i)
!end do

write(*,*) size(A), N
call prueba_lib

end program main