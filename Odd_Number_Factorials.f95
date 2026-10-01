program aa
implicit none 
integer*8:: f,i,j,v


Do i=1,10
  f=1
  j=2*i-1
   do while(j .ge. 1)
    f=f*j
    j=j-1
   end do
write(*,*)f
ENDDo
stop
end program aa