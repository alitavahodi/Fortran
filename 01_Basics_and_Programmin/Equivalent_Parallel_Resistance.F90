program equal_R
f=1
do 35 i=1,999
  R=1+(i-1)*1.5
  if(i>1) then
     f=((1/f)+(1/R))**(-1) 
     end if
 write(*,*)'i=',i,'        R=',R,'         R(e)=',f

35 continue
end program equal_R
