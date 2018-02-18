module algebra_lineal
    implicit none
    contains
    
subroutine Gauss(A,b,x)
    
    ! Argumentos de la subrutina
     real(8), intent(in)    :: A(:,:)    !|
     real(8), intent(in)    :: b(:)      !| Dimensiones asumidas
     real(8), intent(inout) :: x(:)      !|
     ! variables locales
     integer              :: m       ! Dimension del problema A(m,m) b(m) X(m)
     real(8), allocatable :: Ab(:,:) ! Matriz ampliada. Dimension depende de m
     real(8)  :: h
     integer  :: i,j,k
     real(8), allocatable :: C(:)

     m = size(A,1)
     allocate(Ab(m,m+1))
     allocate(C(m+1))
     Ab(1:m,1:m) = A
     Ab(1:m,m+1) = b
    ! Etapa triangulación
    do i = 1, m-1
        if (abs(Ab(i,i))<epsilon(1.d0)) then  !Cero en la diagonal
                C=Ab(i,:)
                do j=i+1, m 
                if (Ab(j,i)>epsilon(1.d0)) then 
                    Ab(i,:)=Ab(j,:)
                    Ab(j,:)=C
                    exit
                endif
                enddo        
        endif
        do k = i+1, m                           ! filas por debajo
        h = Ab(k,i)/Ab(i,i)                     ! Factor que multiplica la fila i
        Ab(k,:) = Ab(k,:) - h*Ab(i,:)     
        enddo
    enddo
 
    ! Fin Triangulación
    ! Etapa sustitución
    do i = m,1,-1

        h = Ab(i,m+1) 	! Guardo en h el valor de la columna ampliada
  
        do j = i+1,m
            h = h-Ab(i,j)*x(j) ! Resto los productos de x's ya calculados
        enddo
  
        x(i) = h/Ab(i,i) 	! Divido por la diagonal
  
    enddo
  
end subroutine Gauss

subroutine Pivotar (A,Id,i)
        
        real *8  :: A (:,:)
        real *8  :: Id (:,:)
        real *8 , allocatable :: T(:) , G(:)
        integer :: k (1)
        integer ::  i , p  , m , n 
        real *8 :: x  , y 
        
        m = size(a,1)
        n = m -i +1 
        allocate (T(n), G(size(a,1)))
    
        ID =  0.d0
    
        !---Creacion de la identidad 
    
            do p = 1, m 
                Id(p,p) = 1 
            end do 
                
            t = a(i:m,i)
                
    
            x = MAXVAL(t)
            y = MINVAL(t)
    
    
            !-----Pivotar
            if (abs(x) > abs(y))then 
    
                k = MAXLOC(T) + i - 1
    
                p = k(1)
    
                g = a(p,:)
                a(p,:) = a(i,:) 
                a(i,:) = g
                
                g = id(p,:)
                id(p,:) = id(i,:)                
                id(i,:) = g
    
            else
                    
                k = minLOC(T) + i - 1
    
            p = k(1)
    
            g = a(p,:)
            a(p,:) = a(i,:) 
            a(i,:) = g 
                
            g = id(p,:)
            id(p,:) = id(i,:)                
            id(i,:) = g 
                    
        end if    
end subroutine
    

subroutine eliminacion(A,Li,m,j)
    real(8), intent(inout) ::A(:,:)
    integer, intent(in) :: m 
    integer, intent(in) :: j
    real(8), intent(inout) ::Li(:,:)
    integer :: i
    real(8) :: h



    do i=1,m
        Li(i,i)=1
    enddo

    do i=j+1,m                           
            h = A(i,j)/A(j,j)  
            Li(i,j)= -h                  
            A(i,:) = A(i,:) - h*A(j,:)     
    enddo   

endsubroutine eliminacion

subroutine inversa(U,m,Ui)
            integer,intent(in)::m 
            integer::i,j
            real*8::k
            real*8,intent(inout)::U(m,m)
            real*8,allocatable::B(:,:)
            real*8,intent(inout)::Ui(m,m)
            real*8::d
            allocate(B(m,2*m))
            d=0
            Ui=0
            B=0
            do i=1,m
                do j=1,m
                    d=0
                    d=U(i,j)
                    B(i,j)=d
                end do 
            end do 
        d=0
            do i=1,m
                do j=m+1,2*m
                    if(i+m==j)then 
                        B(i,j)=1
                    end if 
                end do  
            end do 
            
            !Triangulación Inferior
            
        do i=1,m-1
            !if(abs(B(i,i))<epsilon(1.d0))STOP 
            do j=i+1,m
                k=0
                k=B(j,i)/B(i,i)
                B(j,:)=B(j,:)-k*B(i,:)
            end do 
        end do 
            
        k=0
        !Triangular superior
            
        do i=m,2,-1
            do j=i-1,1,-1
                k=0
                k=B(j,i)/B(i,i)
                B(j,:)=B(j,:)-k*B(i,:)
            end do 
        end do
            
            
        !Dividir para tener la identidad a la izquierda
        do i=1,m
        do j=1,m
                if(i==j)then 
                    k=0
                    k=B(i,j)
                    B(i,:)=B(i,:)/k
                end if 
            end do
        end do 
                
        !Lo pasamos a la matriz inversa como una 3x3
        do i=1,m 
            do j=m+1,2*m
            d=0
            d=B(i,j)
            Ui(i,j-m)=d
            end do 
        end do 
                    
end subroutine inversa 

subroutine aproximar(A,m)
    real(8), intent(inout) :: A(:,:)
    integer, intent(in) :: m
    integer :: i,j 
    do i=1,m
        do j=1,m 
            if(abs(A(i,j))<(1e-10)) then 
                A(i,j)=0.d0
            endif 
        enddo
    enddo
endsubroutine
end module
  