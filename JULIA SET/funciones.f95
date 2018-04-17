module funciones
contains

function jacobiano(X,Y)
    real*8 :: jacobiano(2,2)
    real*8, intent(in) :: X, Y

    jacobiano(1,1) = 3*(X**2) - 3*(Y**2)
    jacobiano(2,1) = 6*X*Y

    jacobiano(1,2) = jacobiano(2,1)
    jacobiano(2,2) = 3*(X**2) - 3*(Y**2)
end function jacobiano

end module funciones