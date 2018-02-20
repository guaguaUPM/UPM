!GOD MODULE
module algebra_lineal 
implicit none

contains

    subroutine gauss (A, b, x)

        ! Argumentos de la subrutina
        real(8), intent(inout) :: A(:,:)     !
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
                gua = max(A(l,i),gua)
                if (gua==A(l,i)) then 
                    y=l
                    guagua=A(l,m)
                endif
            enddo
                A(l,:)=A(i,:)
                A(i,:)=guagua(:)
                write(*,*) gua 
                write(*,*) A

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

    end subroutine

    subroutine triangulacion_superior_gauss (A, At)
        ! Argumentos de la subrutina
        real(8), intent(in)     :: A(:,:)      !
        real(8), intent(out)    :: At(:,:)     !

        ! Variables locales                  
        real(8)                 :: h
        real(8)                 :: maximo               
        real(8),allocatable     :: linea_emax (:)  
        integer                 :: i, k, l, y, m
    
        m = size(A,1) 
        allocate(linea_emax(m-1))
        At = A
    
        ! ETAPA TRIANGULACION
        do i = 1, m-1
            ! INICIO PIVOTE
            ! Tenemos la fila y la columna donde hay un cero, comparamos numeros para hallar el maximo en la misma columna
            maximo = 0
            linea_emax = 0
            if (At(i,i)==0) then 
                do l=i+1,m
                    maximo = max(At(l,i), maximo)
                    if (maximo==At(l,i)) then 
                        y = l
                        linea_emax = At(l,m)
                    endif
                enddo
                At(l,:) = At(i,:)
                At(i,:) = linea_emax(:)
                write(*,*) maximo 
                write(*,*) At
            endif
            ! FIN PIVOTE

            do k = i+1, m                       ! Filas por debajo 
                h = At(k,i) / At(i,i)             ! Factor que multiplica la fila i
                At(k,:) = At(k,:) - h*At(i,:)
            enddo

        enddo
        ! FIN TRIANGULACION
    end subroutine
        
    subroutine factorizacionLU (A, L, U, n)
    
        real(8),intent(in)::A(n,n)
        integer,intent(in)::n
        real(8),intent(out),allocatable:: L(:,:),U(:,:)
        integer::i,j,k
        real(8) :: sum
        
        allocate(L(n,n))
        allocate(U(n,n))
        
        !Aplicamos el algoritmo matemático asociado a la factorización LU
        
        L=0.d0
        U=0.d0
        
        L(1,1) = A(1,1)
        U(1,1) = 1
         
        do k=1,n-1
                                                       !Es importante el orden de los bucles 
            U(1,k+1) = A(1,k+1)/L(1,1)
                  
            do i=2,k      
                do j=1,i-1
                    sum = L(i,j)*U(j,k+1)
                end do 
                U(i,k+1) = (A(i,k+1)-sum)/L(i,i)
            end do  
            
            L(k+1,1) = A(k+1,1)
        
            do i=2,k      
                do j=1,i-1
                    sum = U(j,i)*L(k+1,j)
                end do 
                L(k+1,i) = (A(k+1,i)-sum)
            end do
        
            U(k+1,k+1) = 1                              
           
            do i=1,k
                sum = L(k+1,i)*U(i,k+1)     
            end do
           
            L(k+1,k+1) = A(k+1,k+1)-sum   
        end do
        
    end subroutine

    subroutine pivotar (ENTRADA, SALIDA, B_ENTRADA, B_SALIDA, PERMUTACION, TAMANO)
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
        
    end subroutine

    function inversa (U)
        real*8, intent(in)             :: U(:,:)

        real*8, allocatable            :: inversa(:,:)
        real*8, allocatable            :: B(:,:)
        real*8                         :: d, k
        integer                        :: i, j, n

        n = size(U,1)
        allocate(inversa(n,n))
        allocate(B(n,2*n))
           
        d=0
        inversa=0
        B=0
            
        do i=1, n
            do j=1, n
                d=0
                d=U(i,j)
                B(i,j)=d
            end do 
        end do 
        
        d=0
        do i=1,n
            do j=n+1,2*n
                if (i+n==j) then 
                    B(i,j)=1
                end if 
            end do  
        end do 
            
        !Triangulación Inferior  
        do i=1, n-1
            !if(abs(B(i,i))<epsilon(1.d0)) STOP 
            do j=i+1, n
                k=0
                k=B(j,i)/B(i,i)
                B(j,:)=B(j,:)-k*B(i,:)
            end do 
        end do 
            
        k=0

        !Triangular superior   
        do i=n, 2, -1
            do j=i-1, 1, -1
                k=0
                k=B(j,i)/B(i,i)
                B(j,:)=B(j,:)-k*B(i,:)
            end do 
        end do
            
            
        !Dividir para tener la identidad a la izquierda
        do i=1, n
            do j=1, n
                if (i==j) then 
                    k=0
                    k=B(i,j)
                    B(i,:)=B(i,:)/k
                end if 
            end do
        end do 
                
        !Lo pasamos a la matriz inversa como una 3x3
        do i=1, n
            do j=n+1, 2*n
            d=0
            d=B(i,j)
            inversa(i,j-n)=d
            end do 
        end do 
                    
    end function

    !ITERATIVOS REQUIEREN QUE EL SISTEMA CONVERJA
    !REQUIERE QUE NO EXISTAN CEROS EN LA DIAGONAL (PREVIO PIVOTAMIENTO) 

    subroutine jacobi (A, Xfinal, b, tol)

        ! Argumentos de la subrutina
        real(8), intent(in) :: A(:,:)          !
        real(8), intent(in) :: b(:)            ! DIMENSIONES ASUMIDAS
        real(8), intent(inout) :: Xfinal(:)    !
        real(8), intent(in) :: tol             !
    
        ! Variables locales
        integer :: n                     ! Dimensión del problema A(n,n) b(n) X(n)
        real(8), allocatable :: x0(:)
        real(8), allocatable :: x(:)
        real(8), allocatable :: L(:,:)
        real(8), allocatable :: D(:,:)
        real(8), allocatable :: U(:,:)
        real(8), allocatable :: c(:)
        real(8), allocatable :: T(:,:)
        integer :: iter, j, k, maxiter
    
        n = size(A,1)
        allocate(x0(n))
        allocate(x(n))
        allocate(L(n,n))
        allocate(D(n,n))
        allocate(U(n,n))
        allocate(c(n))
        allocate(T(n,n))
    
        !Establecemos las matrices L, D y U para el algoritmo de la iteración
        do j= 1, n
            do k=1, n
                if (k > j) then
                    U(j,k) = A(j,k)
                else if (k == j) then
                    D(j,k) = A(j,k)
                else 
                    L(j,k) = A(j,k)
                end if
            end do
        end do
            
        !Segun nos indica la formula reducida del método de Jacobi calculamos las matrices c y T
        c = matmul(inversa(D), b)
        T = matmul((-1)*inversa(D),(U+L))
    
        !Primera semilla (primer valor establecido de x para iniciar la iteración)
        x0 = 0.d0
        maxiter = 999999
    
        do iter = 1, maxiter
            x = matmul(T, x0) + c
            if (norma2((x-x0), n)/norma2(x, n) <= tol) then
                Xfinal = x
                stop
            else 
                x0 = x
            end if
        end do
    
    end subroutine
    
    subroutine gauss_seidel (A, Xfinal, b, tol)
    
        ! Argumentos de la subrutina
        real(8), intent(in) :: A(:,:)          !
        real(8), intent(in) :: b(:)            ! DIMENSIONES ASUMIDAS
        real(8), intent(inout) :: Xfinal(:)    !
        real(8), intent(in) :: tol             !
        
        ! Variables locales
        integer :: n                     ! Dimensión del problema A(n,n) b(n) X(n)
        real(8), allocatable :: x0(:)
        real(8), allocatable :: x(:)
        real(8), allocatable :: L(:,:)
        real(8), allocatable :: D(:,:)
        real(8), allocatable :: U(:,:)
        real(8), allocatable :: c(:)
        real(8), allocatable :: T(:,:)
        integer :: iter, j, k, maxiter
        
        n = size(A,1)
        allocate(x0(n))
        allocate(x(n))
        allocate(L(n,n))
        allocate(D(n,n))
        allocate(U(n,n))
        allocate(c(n))
        allocate(T(n,n))
        
        !Establecemos las matrices L, D y U para el algoritmo de la iteración
        do j= 1, n
            do k=1, n
                if (k > j) then
                    U(j,k) = A(j,k)
                else if (k == j) then
                    D(j,k) = A(j,k)
                else 
                    L(j,k) = A(j,k)
                end if
            end do
        end do
                
        !Segun nos indica la formula reducida del método de Jacobi calculamos las matrices c y T
        c = matmul(inversa(D+L), b)
        T = matmul((-1)*inversa(D+L), U)
        
        !Primera semilla (primer valor establecido de x para iniciar la iteración)
        x0 = 0.d0
        maxiter = 999999
        
        do iter = 1, maxiter
            x = matmul(T, x0) + c
            if (norma2((x-x0), n)/norma2(x, n) <= tol) then
                Xfinal = x
                stop
            else 
                x0 = x
            end if
        end do
    end subroutine
    
    function norma2 (vector, n)
        real(8), intent(in) :: vector(:)
        integer, intent(in) :: n
        real(8) :: norma2
        integer :: i
    
        norma2 = 0.d0
        do i = 1, n
            norma2 = norma2+vector(i)**2
        enddo
        norma2 = sqrt(norma2) 
        
    end function
        
end module