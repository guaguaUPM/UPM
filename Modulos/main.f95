program main
use algebra_lineal
implicit none

real(8),allocatable :: MA(:,:)     
real(8),allocatable :: Vb(:)       
real(8),allocatable :: Vx(:)
integer::m=4

allocate (MA(m,m))
allocate (Vb(m))
allocate (Vx(m))

MA(1,1)=1
MA(1,2)=2
MA(1,3)=3
MA(1,4)=10
MA(2,1)=4
MA(2,2)=0
MA(2,3)=6
MA(2,4)=11
MA(3,1)=7
MA(3,2)=8
MA(3,3)=9
MA(3,4)=12
MA(4,1)=13
MA(4,2)=2
MA(4,3)=15
MA(4,4)=16


call jacobi (MA, Vx, Vb, 0.0000004d0)

write(*,*) 'Solución'
write(*,*) Vx
write(*,*) 'Comprobación'
write(*,*) Matmul(Ma,Vx)
write(*,*) Vb

end program main