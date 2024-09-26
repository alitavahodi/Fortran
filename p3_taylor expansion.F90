program taylor
implicit none
integer*8::i,j
double precision::m,l,f,k,t,x
write(*,*)'enter x'
read(*,*)x

t=0
Do i=1,10
  f=1
  
  j=2*i-1
  
  l=x**(2*i-1)
  k=(-1)**(i+1)
  
  
   do while(j .ge. 1)
    f=f*j
    j=j-1
    m=((k*l)/f)
    
   
   enddo
   
   t=t+m
    write(*,*)'i=',i,'            taylor(x)=sin(x)=',t
end do
stop
end program taylor