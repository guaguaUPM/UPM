program main
use algebra_lineal
implicit none
real(8),allocatable :: MA(:,:)     
real(8),allocatable :: Vb(:)       
real(8),allocatable :: Vx(:)
integer::m=3

allocate (MA(m,m))
allocate (Vb(m))
allocate (Vx(m))

MA(1,1)=1
MA(1,2)=1
MA(1,3)=1
MA(2,1)=1
MA(2,2)=1
MA(2,3)=1
MA(3,1)=1
MA(3,2)=1
MA(3,3)=1


call Gauss(MA,Vb,Vx)

    write(*,*) 'Solución'
    write(*,*) Vx
    write(*,*) 'Comprobación'
    write(*,*) Matmul(Ma,Vx)
    write(*,*) Vb



end program main