program cc
implicit none
integer::i,j,k
integer::a(4,5)
real::b(5,8),c(4,8)
open(123,file='a.txt')
open(124,file='b.txt')
open(125,file='c.txt')
 
 do i=1,4
  read(123,*)(a(i,k),k=1,5)
  write(*,"(5I4)")(a(i,k),k=1,5)
 enddo



 do k=1,5
  read(124,*)(b(k,j),j=1,8)
 enddo
c = 0.0
     do i=1,4
		do j=1,8
        	do k=1,5
        c(i,j) =  c(i,j)   +   a(i,k)*b(k,j) 
      enddo
      enddo
      enddo
    
do i=1,4
write(125,"(8F20.12)")(c(i,k),k=1,8)
enddo
close(124)
close(125)
close(123)
stop
end program cc 
