program main
contains algebra_lineal
implicit none
real(8),allocatable :: MA(:,:)     
real(8),allocatable :: Vb(:)       
real(8),allocatable :: Vx(:)
integer::m=3

allocate (MA(m,m))
allocate (Vb(m))
allocate (Vx(m))

call Gauss(MA,Vb,Vx)

    write(*,*) 'Solución'
    write(*,*) Vx
    write(*,*) 'Comprobación'
    write(*,*) Matmul(Ma,Vx)
    write(*,*) Vb



end program main