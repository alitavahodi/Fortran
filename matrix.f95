program cc
implicit none
integer::i,j
integer::a(4,5)
real::b(5,8)
open(123,file='a.txt')
open(124,file='b.txt')
open(125,file='bb.txt')
 !do i=1,4
   !do j=1,5
     !read(12,*)a(i,j)
   !enddo 
  !enddo
 do i=1,4
  read(123,*)(a(i,j),j=1,5)
 enddo

do i=1,4
  write(*,"(5I4)")(a(i,j),j=1,5)
 enddo

 do i=1,5
  read(124,*)(b(i,j),j=1,8)
 enddo

do i=1,4
  write(*,"(8f20.8)")(b(i,j),j=1,8)
 enddo 
close(125)

close(123)
stop
end program cc 
