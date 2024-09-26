program tavahodi
implicit none
real :: k1, k2, k3, k4, f1
real :: L1, L2, L3, L4, f2
real :: M1, M2, M3, M4, f3
real :: Q1, Q2, Q3, Q4, f4
real :: x,y,vx,vy,t,h,a,b,n
real::C,D,C0,D0
integer :: i
!--------------------------

a = 0 .0
b = 20 .0
h = 0.001
n=(b-a)/h
x = 10; y =0
vx = -1 ; vy = 6
t = 0 .0
C0=13**(1/2)
D0=10
open(111,file="x-y.txt")

open(222,file="xyxy.txt")
write(111,'(5f10.5)') x , y
write(222,'(5f10.5)') C0, D0
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
write(111,'(5f10.5)') x , y 
C=sqrt(vx**2+vy**2)
D=sqrt(x**2+y**2)
write(222,'(5f10.5)') C, D
end do
write(*,*)'finished'
close(111)
close(222)
stop
end Program tavahodi
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
f2 = -0.5*x-0.5*y
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
f4 = -0.5*x-2*y
end function