module invertir
contains
    function inversa(ENTRADA)
        real*8, intent(in) :: ENTRADA(:,:)
        real*8, allocatable :: inversa(:,:)
        integer :: tamano, IPIV(:,:), i
        tamano = size(ENTRADA(1,:))
        allocate(inversa(tamano, tamano))
        allocate(IPIV(tamano, tamano))

        inversa = ENTRADA
        call DGETRI(tamano, inversa, tamano, IPIV, tamano, -1, i)
    
    end function inversa
end module invertir