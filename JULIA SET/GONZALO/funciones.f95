module funciones
contains

function jacobiano(X,Y)
    implicit none
    real*8 :: jacobiano(2,2)
    real*8, intent(in) :: X, Y

    jacobiano(1,1) = 3.d0*(X**2) - 3.d0*(Y**2)
    jacobiano(2,1) = -6.d0*X*Y

    jacobiano(1,2) = 6.d0*X*Y
    jacobiano(2,2) = 3d0*(X**2) - 3.d0*(Y**2)
end function jacobiano

function FUNCION1(X,Y)
    implicit none
    real*8 :: FUNCION1
    real*8,intent(in) :: X, Y

    FUNCION1 = X**3 - 3.d0*X*(Y**2) - 1.d0/3.d0
end function FUNCION1

function FUNCION2(X,Y)
    implicit none
    real*8 :: FUNCION2
    real*8,intent(in) :: X, Y
   
    FUNCION2 = 3.d0*(X**2)*Y - Y**3 - 1.d0/3.d0 
end function FUNCION2

end module funciones