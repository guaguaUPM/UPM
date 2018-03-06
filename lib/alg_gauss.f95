subroutine gauss_sustituir(A,B, RESULTADO, TAMANO, MODO)
    ! Se sustituyen los valores del sistema AX=B

    ! CON CEROS EN LA DIAGONAL

    ! Si MODO es TRUE, se interpreta la matriz de entrada como triangular superior:
    ! 1 3 4 | 0.1
    ! 0 1 5 | 0.2
    ! 0 0 1 | 0.3
    ! Si MODO es FALSE, se interpreta como triangular inferior:
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