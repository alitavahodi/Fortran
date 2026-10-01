program question4
implicit none
double precision :: a1, a2 ! dx
double precision :: b1, b2 ! dy
double precision :: c1, c2 ! dz
double precision :: d1, d2 ! dw
double precision :: answer ! results
integer :: n ! number of random_numbers
! -----------------------------
! a1,a2,b1,b2,c1,c2,d1,d2 hodoode integral hastand.
a1 = 0 .0 ; a2 = 1 .0
b1 = -1 .0 ; b2 = 2 .0
c1 = 2 .0 ; c2 = 3 .0
d1 = -1 .0 ; d2 = 3 .0
n = 100000
! -----------------------------
call MonteCarlo( a1, a2, b1, b2, c1, c2, d1, d2,n,answer)
write(*,*) " Answer = " , Answer
! Coded by Ali Farzanehpoor, AliWM@Live.com
stop
end program question4
! -----------------------------
Subroutine MonteCarlo( a1, a2, b1, b2, c1, c2, d1, d2,n,answer)
implicit none
double precision :: r1, r2, r3, r4
double precision :: a1, a2
double precision :: b1, b2
double precision :: c1, c2
double precision :: d1, d2
double precision :: answer,summation
double precision :: aliii
integer :: i, n
summation = 0 .0
do i=1,n
call random_number( r1) ! 0<r1<1
call random_number( r2) ! 0<r2<1
call random_number( r3) ! 0<r3<1
call random_number( r4) ! 0<r4<1
r1 = ( a2 - a1)* r1 + a1 ! a1<r1<a2
r2 = ( b2 - b1)* r2 + b1 ! b1<r2<b2
r3 = ( c2 - c1)* r3 + c1 ! c1<r3<c2
r4 = ( d2 - d1)* r4 + d1 ! d1<r4<d2
summation = summation + aliii( r1, r2, r3, r4)
end do
answer =(1 .0/n)* (( a2- a1)*( b2- b1)*( c2- c1)*( d2- d1))*summation
! Coded by Ali Farzanehpoor, AliWM@Live.com
return
end subroutine
! --------------------------
function aliii(x,y,z,w)
implicit none
double precision :: x,y,z,w,aliii
! Coded by Ali Farzanehpoor, AliWM@Live.com
aliii = SQRT(abs(x*y-y*y+2 .0*y*z-sin(w)))
end function