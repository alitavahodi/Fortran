program QQQ3
implicit none
real :: k1, k2, k3, k4, f1
real :: L1, L2, L3, L4, f2
real :: M1, M2, M3, M4, f3
real :: Q1, Q2, Q3, Q4, f4
real :: x,y,vx,vy,t,h,a,b
integer :: i,n
!--------------------------
n = 10000
a = 0 .0
b = 20 .0
h = (b-a)/n
x = 1 .0 ; y = 0 .0
vx = 0 .0 ; vy = 1 .0
t = 0 .0
open(111,file="results.txt")
write(111,*) " t x y vx vy "
write(111,'(5f10.5)') t, x , y , vx, vy
!--------------------------
do i=1,n
k1 = h * f1( t , x , y , vx , vy )
L1 = h * f2( t , x , y , vx , vy )
M1 = h * f3( t , x , y , vx , vy )
Q1 = h * f4( t , x , y , vx , vy )
!-----------------
k2 = h * f1( t+h/2 .0 , x + k1/2 .0 , y + M1/2 .0 , vx + L1/2 .0 , vy + Q1/2 .0 )
L2 = h * f2( t+h/2 .0 , x + k1/2 .0 , y + M1/2 .0 , vx + L1/2 .0 , vy + Q1/2 .0 )
M2 = h * f3( t+h/2 .0 , x + k1/2 .0 , y + M1/2 .0 , vx + L1/2 .0 , vy + Q1/2 .0 )
Q2 = h * f4( t+h/2 .0 , x + k1/2 .0 , y + M1/2 .0 , vx + L1/2 .0 , vy + Q1/2 .0 )
!-----------------
k3 = h * f1( t+h/2 .0 , x + k2/2 .0 , y + M2/2 .0 , vx + L2/2 .0 , vy + Q2/2 .0 )
L3 = h * f2( t+h/2 .0 , x + k2/2 .0 , y + M2/2 .0 , vx + L2/2 .0 , vy + Q2/2 .0 )
M3 = h * f3( t+h/2 .0 , x + k2/2 .0 , y + M2/2 .0 , vx + L2/2 .0 , vy + Q2/2 .0 )
Q3 = h * f4( t+h/2 .0 , x + k2/2 .0 , y + M2/2 .0 , vx + L2/2 .0 , vy + Q2/2 .0 )
!-----------------
k4 = h * f1( t+h , x + k3 , y + M3 , vx + L3 , vy + Q3 )
L4 = h * f2( t+h , x + k3 , y + M3 , vx + L3 , vy + Q3 )
M4 = h * f3( t+h , x + k3 , y + M3 , vx + L3 , vy + Q3 )
Q4 = h * f4( t+h , x + k3 , y + M3 , vx + L3 , vy + Q3 )
!-----------------
x = x + (1 .0/6 .0)*( k1+(2* k2)+(2* k3)+ k4)
y = y + (1 .0/6 .0)*( M1+(2* M2)+(2* M3)+ M4)
vx = vx + (1 .0/6 .0)*( L1+(2* L2)+(2* L3)+ L4)
vy = vy + (1 .0/6 .0)*( Q1+(2* Q2)+(2* Q3)+ Q4)
T = T + h
write(111,'(5f10.5)') t, x , y , vx, vy
end do
close(111)
stop
end Program QQQ3
!-------------------------
function f1(t,x,y,vx,vy)
implicit none
real :: f1,t,x,y,vx,vy
f1 = vx + 0 .0*(x+y+vy+t)
end function
!-------------------------
function f2(t,x,y,vx,vy)
implicit none
real :: f2,t,x,y,vx,vy
f2 = -x/((x*x+y*y)**(3 .0/2 .0)) + 0 .0*(vx+vy+t)
end function
!-------------------------
function f3(t,x,y,vx,vy)
implicit none
real :: f3,t,x,y,vx,vy
f3 = vy + 0 .0*(x+y+vx+t)
end function
!-------------------------
function f4(t,x,y,vx,vy)
implicit none
real :: f4,t,x,y,vx,vy
f4 = -y/((x*x+y*y)**(3 .0/2 .0)) + 0 .0*(vx+vy+t)
end function