program main
use algebra_lineal
implicit none

real*8, allocatable :: A(:,:), At(:,:), L(:,:), U(:,:), b(:), X(:)
integer :: i, j
real*8 :: tol
integer, parameter :: N=100 !DEPENDE DEL EXAMEN
allocate(A(N,N))
allocate(At(N,N))
allocate(L(N,N))
allocate(U(N,N))
allocate(b(N))
allocate(X(N))

!Sean la matriz A de tamaño 100 x 100 y el vector b de tamaño 100 definidos como:
do i = 1, N
    do j = 1, N
        b(i) = (i*(1.d0))/10
        if (i==j) then
            A(i,j) = 100*(i+j)
        else
            A(i,j) = i-j
        end if
    end do
end do

!A.- Responder a las siguientes cuestiones sobre el sistema lineal AX = B:

    !1. Sea la matriz AT el resultado de aplicar el método de Gauss al sistema AX = B. 

    call triangulargauss (A, At)
    write(*,*) "Escriba la fila y columna de la posición deseada"
    read(*,*) i, j
    write(*,*) "Su resultado es:", A(i,j)
    read(*,*)

    !DEBUG:
    !write(*,*) "Su matriz At resultado es:" 
    !do i = 1, N
        !write(*,*) A(i,:)
    !end do
    !read(*,*)

    !2. Sean las matrices L y U el resultado de hacer una descomposición de tipo A=LU 
    !(descomposición LU sin permutación).

    call factorizacionLU (A, L, U, N)
    write(*,*) "Escriba la fila y columna de la posición deseada de L"
    read(*,*) i, j
    write(*,*) "Su resultado es:", L(i,j)
    read(*,*)
    write(*,*) "Escriba la fila y columna de la posición deseada de U"
    read(*,*) i, j
    write(*,*) "Su resultado es:", U(i,j)
    read(*,*)
    
    !DEBUG:
    !write(*,*) "Su matriz L resultado es:" 
    !do i = 1, N
        !write(*,*) A(i,:)
    !end do
    !write(*,*) "Su matriz U resultado es:" 
    !do i = 1, N
        !write(*,*) A(i,:)
    !end do

    !3. Resolver el sistema AX = B utilizando el método de Gauss, 
    !    la descomposición LU y los métodos iterativos de Jacobi y de Gauss-Seidel 
    !    (con el vector 𝑥 = (0,0, ... ,0,0) como condición inicial).
     
    write(*,*) "Escriba un valor para la tolerancia de gauss-seidel y jacobi"
    read(*,*) tol

    !Call cada una de las funciones   
    call gauss (A, b, X)
    write(*,*) "Xi de Gauss", X
    read(*,*)
 
    !call resoluciónLU (L, U, X)
    !write(*,*) "Xi de Gauss", X
    !read(*,*)    

    call jacobi (A, X, b, tol)
    write(*,*) "Xi de Jacobi", X
    read(*,*)

    call gauss_seidel (A, X, b, tol)
    write(*,*) "Xi de Gauss-Seidel", X
    read(*,*)


!B.- Realizar el ajuste por mínimos cuadrados de los datos contenidos 
!     en el fichero data_file.txt que podéis descargar de Moodle.

end program main