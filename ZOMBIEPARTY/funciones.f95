module variables

contains 

function zombis (a,b,ze,d,T,dt)
!A es el ratio de destrucción de zombis
!b es el ratio de aparición de nuevos zombis
!d es el ratio de muerte natural
!T tiempo en el que se termina 
!dt ...
!valores iniciales
N = 500            !numero de personas, se puede cambiar
n = T/dt
t = zeros(1,n+1)   !los llenamos de ceros
s = zeros(1,n+1)
z = zeros(1,n+1)
r = zeros(1,n+1)

s(1) = N 
z(1) = 0
r(1) = 0
t = 0 !debe ir de 0 a T con un incremento de dt

do i= 1,n 
    s(i+1) = s(i) + dt*(-b*s(i)*z(i)) !esto se ha hecho asumiendo que la tasa de nacimiento es igual a la tasa de muerte, por eso no aparecen 
    z(i+1) = z(i) + dt*(b*s(i)*z(i) - a*s(i)*z(i) + ze*r(i)) !zombis mas infectados menos eliminados naturalmente mas tasa de aparicion
    r(i+1) = r(i) + dt*(a*s(i)*z(i) + d*s(i) - ze(r)) !eliminados mas destruccion de zombis mas muertes naturales menos resucitados

    if (s(i) < 0 .OR. s(i) > N) then  !en el primer caso han muerto todos, en el segundo hemos ganado o es una situacion imposible 
        exit  
    endif 
    if (z(i) > N .OR. z(i) < 0) then  !en el primer caso ganan los zombis, en el segundo los supervivientes 
        exit 
    endif
    if (r(i) < 0 .OR. r(i) > N) then  !el primer caso es imposible y en el segundo creo que todos mueren, empate
        exit
    endif 
enddo 

hold on 
plot(t,s,’b’) 
plot(t,z,’r’)
legend(’Suscepties’,’Zombies’)
