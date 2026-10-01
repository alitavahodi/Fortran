program a
implicit none
double precision::lower,upper,answerT,answerS,EsSimp
integer::N5,k
N(1)=10
N(2)=100
N(3)=1000
N(4)=10000
N(5)=100000
N(6)=100000000
lower=-50
upper=50
open(11,file='simson.txt')
open(22,file='trapizoidal.txt')
do k=1,6
  call trapezoid(lower,upper,N(k),answerT)
  call simson(lower,upper,N(k),answerS)
  
write(11,'(I10,F30.25)')N(k),answerS,EsSimp

write(22,'(I10,F30.25)')N(k),answerT
end do
close(11)
close(22)
stop
end program a

subroutine trapezoid(a,b,m,ans)
implicit none
double precision::a,b,h,ans
double precision::fun1,x
integer::m,i
h=(b-a)(1*m)
ans=0
 do i=0,m-1
   x=a+i*h
   ans=ans+(h*fun1(x))
   end do
   ans=ans+(h/2)*(fun1(b)-fun1(a))
   return
 end subroutine trapezoid
 
subroutine simpson(a,b,m,ans)
implicit none
double precision::a,b,h,ans,sum1,sum2
double precision::fun1,x
integer::m,i
h=(b-a)(1*m)
sum1=0
sum2=sum1
if (mod(m,2) .ne. 0)then
  exit
  else
    do i=0,(m/2)-1
      x=a+2*i*h
      sum2=2*sum2
      ans=(h/3)*(fun1(a)+sum1+sum2+fun1(b))
      end if
      EsSimp=((upper-lower)/180))*(h**4)*m
      return
      end subroutine
 function fun1(x)
 implicit none
 double precision::x,fun1,p
 p=acos(-1.0)
 fun1(1/sqrt(p))*exp(-x**2)
 end function
      