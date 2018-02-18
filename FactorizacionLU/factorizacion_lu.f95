module factorizacion_lu
contains

subroutine factorizar(ENTRADA, LU, TAMANO)
    implicit none

    ! La matriz entrada ya ha pasado por el pivote, no tiene ceros en la diagonal

    real*8, intent(in)                ::  ENTRADA(:,:)
    real*8, allocatable, intent(out)  ::  LU(:,:)
    integer, intent(in)               ::  TAMANO

    integer                           ::  i, j, k, s
    real*8                            ::  suma

    allocate(LU(tamano, tamano))
    LU = ENTRADA

    ! Algoritmo de factorización
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

    real*8, intent(in)                :: B(:), LU(:,:)
    real*8, intent(out), allocatable  :: X(:)
    integer, intent(in)               :: TAMANO

    real*8, allocatable               :: y(:)
    real*8                            :: suma
    integer                           :: i, j, s

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

subroutine pivotar(ENTRADA, SALIDA, B_ENTRADA, B_SALIDA, PERMUTACION, TAMANO)
    implicit none

    real*8, intent(in)                :: entrada(:,:), b_entrada(:)
    real*8, allocatable, intent(out)  :: salida(:,:) , b_salida(:)
    integer, allocatable, intent(out) :: permutacion(:,:)
    integer, intent(in)               :: tamano

    integer                           :: i, fila_max, j
    real*8                            :: valor_max, b_auxiliar
    real*8, allocatable               :: AUXILIAR(:)
    integer, allocatable              :: permutacion_auxiliar(:)

    allocate(salida(tamano, tamano))
    allocate(b_salida(tamano))
    allocate(permutacion(tamano, tamano))
    allocate(AUXILIAR(tamano))
    allocate(permutacion_auxiliar(tamano))
    
    SALIDA=ENTRADA
    B_SALIDA = B_ENTRADA

    permutacion = 0
    do i=1, tamano
        permutacion(i,i) = 1
    end do

    ! Bucle que recorre la diagonal
    do i=1, tamano
        ! Si se encuetra un 0 en la diagonal
        if(salida(i,i) == 0.d0) then
            ! Resetea el valor maximo, se usarán siempre numeros positivos
            valor_max = 0.d0
            ! Se recorren las posiciones por debajo de la diagonal para encontrar el maximo valor
            do j=i+1, tamano
                if( ABS(SALIDA(j,i)) >= valor_max ) then
                    fila_max = j
                    valor_max = salida(j,i)
                end if
            end do

            ! write(*,*) "FILA MAX:", fila_max, valor_max  ! DEBUG

            ! write(*,*) "CAMBIO", i, "por", fila_max      ! DEBUG

            ! Cambio por rotacion triangular de la matriz
            AUXILIAR(:)        = salida(i,:)
            salida(i,:)        = salida(fila_max, :)
            salida(fila_max,:) = AUXILIAR(:)

            ! Cambio por rotacion triangular de la matriz permutacion
            permutacion_auxiliar(:)   = PERMUTACION(i,:)
            PERMUTACION(i,:)          = PERMUTACION(fila_max,:)
            PERMUTACION(fila_max,:)   = permutacion_auxiliar(:)

            ! Cambio por rotacion triangular del vector B
            b_auxiliar         = b_salida(i)
            b_salida(i)        = b_salida(fila_max)
            b_salida(fila_max) = b_auxiliar

            ! write(*,*) "AUXILIAR vale", AUXILIAR   ! DEBUG
        end if
    end do

end subroutine pivotar
end module factorizacion_lu