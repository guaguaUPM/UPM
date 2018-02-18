module array_randomizer
contains
    subroutine randomizar_matriz(SALIDA, TAMANO)
        real*8, intent(out)  :: SALIDA(:,:)
        integer, intent(in)  :: TAMANO
        integer              :: i, j

        call RANDOM_SEED()
        do i=1, TAMANO
            do j=1, TAMANO
                call RANDOM_NUMBER(HARVEST=SALIDA(i,j))
            end do
        end do
    end subroutine randomizar_matriz

    subroutine randomizar_vector(SALIDA, TAMANO)
        real*8, intent(out)  :: SALIDA(:)
        integer, intent(in)  :: TAMANO
        integer              :: i
    
        call RANDOM_SEED()
        do i=1, TAMANO
            call RANDOM_NUMBER(HARVEST=SALIDA(i))
        end do
    end subroutine randomizar_vector
end module array_randomizer