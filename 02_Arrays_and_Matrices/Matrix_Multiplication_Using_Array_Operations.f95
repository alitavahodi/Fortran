program cc
implicit none
integer::i,j,k,t
integer::a(4,5)
real::b(5,8),c(4,8)
open(123,file='a.txt')
open(124,file='b.txt')
open(125,file='bb.txt')
 
 do i=1,4
  read(123,*)(a(i,k),k=1,5)
  write(*,"(5I4)")(a(i,k),k=1,5)
 enddo



 do k=1,5
  read(124,*)(b(k,j),j=1,8)
   write(125,"(8f20.8)")(b(k,j),j=1,8)
 enddo

t=0
     do i=1,4
         read(123,*)(a(i,k),k=1,5)
         read(124,*)(b(k,j),j=1,8)
 
        c(i,j)=t+(a(i,:)*b(:,j)) 

    
        write(*,'(20f20.20)')c(i,j)
      enddo
     
    



close(125)

close(123)
stop
end program cc 
