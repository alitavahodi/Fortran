program question1
implicit none
integer :: j, i
integer, parameter :: k = 5 ! besurate delkhah, matris-ha ro 5x5 dar nazar gereftam.
double precision :: A(k,k), B(k,k), C(k,k)
! ----------------------
open(1993,file="M1.txt") ! file-ha alaki hastand - masalan darim az file M1.txt mikhoonim
open(1371,file="M2.txt") ! file-ha alaki hastand - masalan darim az file M2.txt mikhoonim
! ----------------------
A = 0 .0
B = 0 .0
do i=1,k
read(1993,*) (A(i,j),j=1,k)
read(1371,*) (B(i,j),j=1,k)
end do
! ----------------------
do i=1,k
do j=1,k
if(A(i,j) .LT. 0.001) then
A(i,j) = 0 .0
end if
if(B(i,j) .LT. 0.001) then
B(i,j) = 0 .0
end if
end do
end do
! ----------------------
C = 0 .0
Call MultipleMatrix(A,B,C)
! Coded by Ali Farzanehpoor, AliWM@Live.com
! ----------------------
write(*,*) " Multiplication of A and B is C "
write(*,*) " C = "
do i=1,k
write(*,"(71f18.5)") (C(i,j),j=1,k)
end do
! ----------------------
close(1993)
close(1371)
stop
end program question1
! ----------------------
Subroutine MultipleMatrix(P,Q,N)
implicit none
integer, parameter :: z = 5
double precision :: P(z,z), Q(z,z), N(z,z)
integer :: i,j,k
! Coded by Ali Farzanehpoor, AliWM@Live.com
N = 0 .0
do i=1,z
do j=1,z
do k=1,z
N(i,j) = N(i,j) + P(i,k) * Q(k,j)
end do
end do
end do
Return
end subroutine
