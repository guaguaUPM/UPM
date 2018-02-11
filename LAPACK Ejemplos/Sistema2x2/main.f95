program pruebas
implicit none

integer :: INFO
real*8 :: A(2,2), B(2,1), IPIV(2,2)

A(1,1) = 1.d0
A(1,2) = 1.d0
A(2,1) = 0.d0
A(2,2) = 2.d0

B(1,1) = 2.d0
B(2,1) = 3.d0 

write(*,*) A(1,:), "|", B(1,:)
write(*,*) A(2,:), "|", B(2,:)

call dgesv(2, 1, A, 2, IPIV, B, 2, INFO)

write(*,*) A(1,:), "|", B(1,:)
write(*,*) A(2,:), "|", B(2,:)

write(*,*) IPIV(1,:)
write(*,*) IPIV(2,:)

write(*,*) info


end program pruebas