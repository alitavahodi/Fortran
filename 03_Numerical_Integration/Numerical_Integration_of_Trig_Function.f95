program mm
implicit none
double precision::h
integer::n,L
real::ae,p,x,f
L=5
p=3.14


read(*,*)x

call I(x)







subroutine I(a)
real::x
real::a,m
m=n-1
L=5
p=3.14
h=L/n
f=f(x)=0
 do x=0,m
   I=h*(f+f(x))
 end do

return
end subroutine I
  



!f(x)
real function f(a)
implicit none
real::a,L,p
L=5
p=3.14
f=(sin(2pa/L))*(sin(4pa/L))*(2/L)
end function f(a)