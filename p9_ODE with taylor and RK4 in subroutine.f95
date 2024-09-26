
Program rbn
implicit none 
double precision:: X0, V0, h 
integer::t0,t10

t0 = 0 ; t10 = 10;  h = 0.02 ;  V0 = 2.0;  X0 = -3.7 
call runge (t0,t10,h,V0,X0)

t0 = 0 ; t10 = 10;  h = 0.020 ;  V0 = 2.0;  X0 = -3.7 

call taylor(t0,t10,h,X0,V0)


stop
endprogram rbn

subroutine runge (a,b,h,V,X)
implicit none 
double precision:: F1,F2,t,V,X,h
double precision:: K1,K2,K3,K4
double precision:: L1,L2,L3,L4
 integer:: I,N,a,b

V = 2.0 ; X = -3/7 ; T= a
N = (b-a)/h    


open(222,file='runge-vx.txt')
open  (123,file="runge_XT.txt")
open  (456,file="runge_VT.txt")
do I=1,N
  
K1 = h*F1(t,X,V)
L1 = h*F2(t,X,V)
!---------------------------------
K2 = h*F1(t+h/2.0,X+K1/2.0,V+L1/2.0)
L2 = h*F2(t+h/2.0,X+K1/2.0,V+L1/2.0)
!------------------------------------
K3 = h*F1(t+h/2.0,X+K2/2.0,V+L2/2.0)
L3 = h*F2(t+h/2.0,X+K2/2.0,V+L2/2.0)
!----------------------------------------
K4 = h*F1(t+h,X+K3 ,V+L3)
L4 = h*F2(t+h,X+K3 ,V+L3)
!-------------------------------------------
X = X+(1.0/6.0)*(K1+2*K2+2*K3+K4)
V = V+(1.0/6.0)*(L1+2*L2+2*L3+L4)
t = t + H

write(123,*) x,t
write(456,*) v,t
write(222,*) x,v
enddo
close(222)
close(123)
close(456)
return
endsubroutine
function F1(t,X,V)
implicit none  
double precision   ::V,F1,X,t
F1 = V

return
endfunction F1



function   F2(t,X,V)
implicit none  
double precision   :: F2, X,V,t
F2= -(0.5*V)-X

return
endfunction F2



subroutine taylor(a,b,h,V,X)
double precision :: t, X, V, h
double precision ::X1,X2,X3, V1,V2,V3
integer :: I, N, A, B
open (666,file="taylor_vx.txt")
open (321,file="taylor_XT.txt")
open (654,file="taylor_VT.txt")

N=(b-a)/h
T=a
do I=0,N
  X1=V
  V1=-0.5*V-X
  !------------
  X2=V1
  V2=-0.5*V1-X1
  !--------------
  X3=V2
  V3=-0.5*V2-X2
  !-------------------
  X=X + h*(X1 + (h/2.0)*(X2+(h/3.0)*X3))
  V=V + h*(V1 + (h/2.0)*(V2+(h/3.0)*V3))
  t=t+h
write (666,*) x, v	  
write (321,*) x, t
write(654,*) v, t
  
enddo
close(666)
close(321)
close(654)
return
end subroutine
