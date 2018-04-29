module funciones
contains

! Funciones problema. Modificar la definicion matematica, dejando los nombres 'funcion' y 'jacobiano'
! Al ser el Grupo 3, C = 1/3

subroutine funcion(X,F)
    implicit none
    real*8, intent(in)  :: X(2)
    real*8, intent(out) :: F(2)

    F(1) = X(1)**3 - 3.d0*X(1)*(X(2)**2) - 1.d0/3.d0
    F(2) = 3.d0*(X(1)**2)*X(2) - X(2)**3 - 1.d0/3.d0
end subroutine funcion

subroutine jacobiano(X,J)
    implicit none
    real*8, intent(out) :: J(2,2)
    real*8, intent(in)  :: X(2)

    J(1,1) = 3.d0*(X(1)**2) - 3.d0*(X(2)**2)
    J(2,1) = 6.d0*X(1)*X(2)

    J(1,2) = -6.d0*X(1)*X(2)
    J(2,2) = 3d0*(X(1)**2) - 3.d0*(X(2)**2)
end subroutine jacobiano

end module funciones