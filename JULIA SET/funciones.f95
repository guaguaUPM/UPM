module funciones
contains

function jacobiano(X,Y)
    implicit none
    real*8 :: jacobiano(2,2)
    real*8, intent(in) :: X, Y

    jacobiano(1,1) = 3*(X**2) - 3*(Y**2)
    jacobiano(2,1) = 6*X*Y

    jacobiano(1,2) = jacobiano(2,1)
    jacobiano(2,2) = 3*(X**2) - 3*(Y**2)
end function jacobiano

function F(X,Y)
    implicit none
    real*8 :: F(2)
    real*8,intent(in) :: X, Y

    F(1) = X**3 - 3*X*(Y**2) - 1.d0/3.d0
    F(2) = 3*(X**2)*Y - Y**3 
end function F

end module funciones