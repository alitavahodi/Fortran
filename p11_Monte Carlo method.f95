program montkarlo
implicit none
double precision::pi,upt,javab
integer::n(4),i,upr,lowr,lowt


pi=acos(-1.0)
 n(1)=5
 n(2)=100
 n(3)=1000 
 n(4)=10**9
upr=4
lowr=0
upt=2.0*pi
lowt=0
open(123,file='MC_results.txt')
  do i=1,4
    call rbn(n(i),upr,lowr,upt,lowt,javab)

     write(123,*) n(i),javab

   end do
close(123)
stop

end program montkarlo
!rrrrrrrrrrrrrrrrrrrrrrrrrrrr!

subroutine rbn(m,upr,lowr,upt,lowt,pasokh)
implicit none
double precision::upt,pasokh,sum,reza,r1,r2
integer::m,i,upr,lowr,lowt

call random_seed()
sum=0.0
do i=1,m
  call random_number(r1)
  call random_number(r2)

  r1=(upr-lowr)*r1+lowr
  r2=(upt-lowt)*r2-lowt

  sum=sum+reza(r1)

end do

pasokh=(upr-lowr)*(upt-lowt)*(1.0/m)*sum

return
end subroutine  rbn

!hhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhh!
function reza(r)
implicit none  
double precision::r,reza
reza=r

end function reza
  
