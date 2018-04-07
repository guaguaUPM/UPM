module lib_gauss
implicit none
contains

subroutine gauss_sustituir(A,B, RESULTADO, TAMANO, MODO)
    ! Se sustituyen los valores del sistema AX=B

    ! CON CEROS EN LA DIAGONAL

    ! Si MODO es .TRUE., se interpreta la matriz de entrada como triangular superior:
    ! 1 3 4 | 0.1
    ! 0 1 5 | 0.2
    ! 0 0 1 | 0.3
    ! Si MODO es .FALSE., se interpreta como triangular inferior:
    ! 1 0 0 | 0.1
    ! 3 1 0 | 0.2
    ! 4 5 1 | 0.3

    implicit none
    integer, intent(in)  :: TAMANO
    real*8, intent(in)   :: A(TAMANO,TAMANO), B(TAMANO)
    real*8, intent(out)  :: RESULTADO(TAMANO)
    logical, intent(in)  :: MODO
    integer              :: i, j

    if (modo) then
        do i = TAMANO, 1, -1
            RESULTADO (i) = B(i)
            do j = i+1, TAMANO
                RESULTADO(i) = RESULTADO(i) - A(i,j) * RESULTADO(j)
            end do
        end do
    else
        do i = 1, TAMANO
            RESULTADO(i) = B(i)
            do j = 1, i-1
                RESULTADO(i) = RESULTADO(i) - A(i,j) * RESULTADO(j)
            end do
        end do
    end if
end subroutine gauss_sustituir

subroutine gauss_triangular(ENTRADA, SALIDA, VECTOR_IN, VECTOR_OUT, TAMANO)
    ! Convierte un sistema AX=B de la forma:
    ! 1 2 3 | 0.1
    ! 4 5 6 | 0.2
    ! 7 8 9 | 0.3
    ! en:
    ! 1 ? ? | ?
    ! 0 1 ? | ?
    ! 0 0 1 | ?
    implicit none
    real*8, intent(in)      :: ENTRADA(TAMANO,TAMANO), VECTOR_IN(TAMANO)
    real*8, intent(out)     :: SALIDA(TAMANO,TAMANO), VECTOR_OUT(TAMANO)
    integer, intent(in)     :: TAMANO

    integer                 :: i, j
    real*8                  :: argumento
    SALIDA = ENTRADA
    VECTOR_OUT = VECTOR_IN

    do i = 1, TAMANO-1
        do j = i+1, TAMANO
            argumento = SALIDA(j,i) / SALIDA(i,i)
            SALIDA(j,:) = SALIDA(j,:) - argumento*SALIDA(i,:)
            VECTOR_OUT(j) = VECTOR_OUT(j) - argumento*VECTOR_OUT(i)
        enddo
    enddo
    ! Se divide la diagonal por su módulo
    do i = 1, TAMANO
        VECTOR_OUT(i) = VECTOR_OUT(i) / SALIDA(i,i)
        SALIDA(i,:) = SALIDA(i,:) / SALIDA(i,i)
    end do

end subroutine gauss_triangular

subroutine resolver_gauss(A,B,X,N)
    implicit none
    integer, intent(in) :: N
    real*8, intent(in) :: A(N,N), B(N)
    real*8, intent(out) :: X(N)
    real*8, allocatable :: A_PIVOT(:,:), B_PIVOT(:)
    integer, allocatable :: PIVOTE(:,:)
    allocate(A_PIVOT(N,N))
    allocate(B_PIVOT(N))
    allocate(PIVOTE(N,N))

    call pivotar(A, A_PIVOT, B, B_PIVOT, PIVOTE, N)
    call gauss_triangular(A_PIVOT, A_PIVOT, B_PIVOT, B_PIVOT, N)
    call gauss_sustituir(A_PIVOT, B_PIVOT, X, N, .TRUE.)
    X = matmul(PIVOTE, X)
end subroutine resolver_gauss

subroutine pivotar (A_ENTRADA, A_SALIDA, B_ENTRADA, B_SALIDA, PIVOTE, TAMANO)
    ! Evita 0 en la diagonal aplicando una pivotacion con operaciones de Gauss, y tambien a un vector que acompañe a la matriz
    ! Para deshacer el pivote: matmul(PIVOTE, MATRIZ_PIVOTADA)
    implicit none
    
    integer, intent(in)               :: TAMANO
    real*8,  intent(in)               :: a_entrada(TAMANO,TAMANO), b_entrada(TAMANO)
    real*8,  intent(out)              :: a_salida(TAMANO,TAMANO) , b_salida(TAMANO)
    integer, intent(out)              :: pivote(TAMANO,TAMANO)
        
    integer                           :: i, fila_max, j
    real*8                            :: valor_max, b_auxiliar
    real*8, allocatable               :: AUXILIAR(:)
    integer, allocatable              :: pivote_auxiliar(:)
        
    
    allocate(AUXILIAR(tamano))
    allocate(pivote_auxiliar(tamano))
            
    A_SALIDA = A_ENTRADA
    B_SALIDA = B_ENTRADA
        
    pivote = 0
    do i=1, tamano
        pivote(i,i) = 1
    end do
        
    ! Bucle que recorre la diagonal
    do i=1, tamano
        ! Si se encuetra un 0 en la diagonal
        if(a_salida(i,i) == 0.d0) then
            ! Resetea el valor maximo, se usarán siempre numeros positivos
            valor_max = 0.d0
            ! Se recorren las posiciones por debajo de la diagonal para encontrar el maximo valor
            do j=i+1, tamano
                if( ABS(A_SALIDA(j,i)) >= valor_max ) then
                    fila_max = j
                    valor_max = a_salida(j,i)
                end if
            end do
        
            ! write(*,*) "FILA MAX:", fila_max, valor_max  ! DEBUG
        
            ! write(*,*) "CAMBIO", i, "por", fila_max      ! DEBUG
        
            ! Cambio por rotacion triangular de la matriz
            AUXILIAR(:)          = a_salida(i,:)
            a_salida(i,:)        = a_salida(fila_max, :)
            a_salida(fila_max,:) = AUXILIAR(:)
    
            ! Cambio por rotacion triangular de la matriz permutacion
            pivote_auxiliar(:)   = pivote(i,:)
            pivote(i,:)          = pivote(fila_max,:)
            pivote(fila_max,:)   = pivote_auxiliar(:)
        
            ! Cambio por rotacion triangular del vector B
            b_auxiliar         = b_salida(i)
            b_salida(i)        = b_salida(fila_max)
            b_salida(fila_max) = b_auxiliar
        end if
    end do
end subroutine pivotar
end module lib_gauss