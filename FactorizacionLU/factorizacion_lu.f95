module factorizacion_lu
contains

subroutine factorizar(ENTRADA, LU, TAMANO)
    implicit none

    ! La matriz entrada no debe tener ceros en la diagonal

    real*8, intent(in)   ::  ENTRADA(:,:)
    real*8, allocatable, intent(out)  ::  LU(:,:)
    integer, intent(in)  ::  TAMANO
    integer              ::  i, j, k, s
    real*8               ::  suma

    allocate(LU(tamano, tamano))
    LU = ENTRADA

    do j = 1, tamano
        do i = 1, j
          if (i == 1) then
            LU(i,j) = LU(i, j)
          else
             suma = 0.0d0
            do k = 1, i-1
               suma = suma + LU(i,k)*LU(k,j)
            end do
                LU(i,j) = LU(i,j) - suma
          end if
        end do
        if ( j < tamano) then
          do s = 1, tamano-j
            i = j + s
            if (j == 1) then
                LU(i,j) = LU(i,j)/LU(j,j)
            else
              suma = 0.0d0
              do k = 1, j-1
                suma = suma + LU(i,k)*LU(k,j)
              end do
                LU(i,j) = (LU(i,j) - suma)/LU(j,j)
            end if
          end do
        end if
       end do

end subroutine factorizar

subroutine resolver(B, LU, X, TAMANO)
    implicit none
    real*8, intent(in) :: LU(:,:), B(:)
    real*8, intent(out), allocatable :: X(:)
    real*8, allocatable :: y(:)
    real*8 :: suma
    integer, intent(in) :: tamano
    integer :: i, j, s

    allocate(x(tamano))
    allocate(y(tamano))


    y(1) = b(1)
    do i = 2, tamano
        suma = 0.0
        do j = 1, i-1
            suma = suma + LU(i,j)*y(j)
        end do
        y(i) = b(i) - suma
    end do
    i = tamano
    j = tamano
    x(i) = y(i)/LU(i,j)
    do s = 1, tamano -1
        i = tamano - s
        suma = 0.0
        do j = i+1, tamano
            suma = suma + LU(i,j)*x(j)
        end do
        x(i) = (y(i) - suma)/LU(i,i)
    end do
end subroutine resolver
end module factorizacion_lu