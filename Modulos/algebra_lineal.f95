module algebra_lineal 
implicit none
! PIPO MUERE
contains
    subroutine Gauss(A,b,x)

        ! Argumentos de la subrutina
        real(8), intent(in) :: A(:,:)     !
        real(8), intent(in) :: b(:)       ! DIMENSIONES ASUMIDAS
        real(8), intent(inout) :: x(:)    !
    
        ! Variables locales
        integer :: m                     ! Dimensión del problema A(m,m) b(m) X(m)
        real(8), allocatable :: Ab(:,:)  ! Matriz ampliada. Dimension depende de m 
        real(8) :: h
        real(8):: gua
        real(8),allocatable:: guagua(:)
        integer :: i,j,k,l,y

        m = size(A,1) 
        allocate(Ab(m,m+1))
        allocate(guagua(m-1))
        Ab(1:m,1:m) = A
        Ab(1:m,m+1) = b

        ! Etapa triangulación
        do i = 1, m-1
            !if (abs(Ab(i,i))<epsilon(1.d0))   "Cero en la diagonal"  !!!!!PIVOTE PARCIAL

            !tenemos la fila y la columna donde hay un cero, comparamos numeros para hallar el maximo en la misma columna
            gua=0
            guagua=0
            if (AB(i,i)==0) then 
            do l=i+1,m
                gua = max(Ab(l,i),gua)
                if (gua==Ab(l,i)) then 
                    y=l
                    guagua=Ab(l,m)
                endif
            enddo
                Ab(l,:)=Ab(i,:)
                Ab(i,:)=guagua(:)
                write(*,*) gua 
                write(*,*) Ab

            do k = i+1, m                       ! Filas por debajo 
                h = Ab(k,i)/Ab(i,i)             ! Factor que multiplica la fila i
                Ab(k,:) = Ab(k,:) - h*Ab(i,:)
            enddo
            endif
        enddo
        ! Fin Triangulación

        ! Etapa sustitución do i = m,1,-1
        do i = m,1,-1
            h = Ab(i,m+1)              ! Guardo en h el valor de la columna ampliada
            do j = i+1,m
                h = h-Ab(i,j)*x(j)     ! Resto de los productos de x´s ya calculados
            enddo
            x(i) = h/Ab(i,i)
        enddo
        ! Fin sustitución

    end subroutine Gauss 

end module