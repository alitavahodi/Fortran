program kk
implicit none

Real, allocatable :: a(:)
integer::i,s,z,n
real::average,std


write(*,*)'n='
read(*,*)n
allocate(a(n))

 open(12,file='random.txt')
   do i=1,n
    read(12,*)a(i)
   end do
   
s=0
  do i=1,n
   s=s+a(i)
  end do
average=s/n
write(*,*)'average=',average
z=0 
   do i=1,n
     z=z+((a(i)-average)**2)
     end do
   std=(z/(n-1))**(1/2)
   write(*,*)'std=',std
stop
end program kk