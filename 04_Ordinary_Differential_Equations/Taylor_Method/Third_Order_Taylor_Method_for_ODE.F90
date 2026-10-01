program P8tavahodibeyk
implicit none
double precision :: h,Yf

integer::Xi,Xf
Xi=0
Xf=3
H= -0.001
Yf= -2.0/5.0

write(*,*)'PLZ W8...'


call Tay(Xi,Xf,H,Yf)
write(*,*)'finisihed'
stop
endprogram P8tavahodibeyk

subroutine Tay(A,B,H,Y)


implicit none
double precision ::H,Y,Y1,Y2,Y3,X,PI
integer::N,A,B,i
PI=acos(-1.0)
N=(A-B)/H
X=3
open(77,file="y-x.txt")
do i=1,N

  Y1=3*X-sin(X+((2*PI)/3))+Y

  Y2=3-cos(X+((2*PI)/3))+Y1

  Y3=sin(X+((2*PI)/3))+Y2

  Y=Y+H*(Y1+H/2.0*(Y2+(H/3.0)*Y3))

  X=X+H
write(77,*) Y,X
enddo
close(77)
return
end subroutine
 