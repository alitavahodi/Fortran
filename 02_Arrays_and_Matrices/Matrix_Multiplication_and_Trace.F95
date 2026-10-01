program QQQ1
implicit none
integer :: j, i
integer, parameter :: z = 5
double precision :: A(z,z), B(z,z), C(z,z)
DOUBLE PRECISION :: TRA, TRB, TRC
! ----------------------
open(1993,file="M1.txt")
open(1371,file="M2.txt")
! ----------------------
A = 0 .0
B = 0 .0
do i=1,z
read(1993,*) (A(i,j),j=1,z)
read(1371,*) (B(i,j),j=1,z)
end do
! ----------------------
C = 0 .0
Call MultipleMatrix(A,B,C)
! Coded by Ali Farzanehpoor, @AliWM@Live.com
! ----------------------
write(*,*) " Multiplication of A and B is C "
write(*,*) " C = "
do i=1,z
write(*,"(100f18.5)") (C(i,j),j=1,z)
end do
! ----------------------
TRA = 0 .0
TRB = 0 .0
TRC = 0 .0
CALL TRACE(A,TRA)
CALL TRACE(B,TRB)
CALL TRACE(C,TRC)
WRITE(*,*) " TRACE OF MATRIX A = ", TRA
WRITE(*,*) " TRACE OF MATRIX B = ", TRB
WRITE(*,*) " TRACE OF MATRIX C = ", TRC
CLOSE(1993)
CLOSE(1371)
stop
end program QQQ1
! ----------------------
Subroutine MultipleMatrix(P,Q,N)
implicit none
integer, parameter :: b = 5
double precision :: P(b,b), Q(b,b), N(b,b)
integer :: i,j,k
! Coded by Ali Farzanehpoor, AliWM@Live.com
N = 0 .0
do i=1,b
do j=1,b
do k=1,b
N(i,j) = N(i,j) + P(i,k) * Q(k,j)
end do
end do
end do
Return
end subroutine
SUBROUTINE TRACE(S,TRR)
IMPLICIT NONE
INTEGER, PARAMETER :: Y = 5
DOUBLE PRECISION :: S(y,y), TRR
INTEGER :: I
TRR = 0 .0
DO I=1, Y
TRR = TRR + S(I,I)
END DO
RETURN
END SUBROUTINE TRACE