program Ali_Farzanehpoor
implicit none
double precision,allocatable :: Rho(:,:) ! charge distribution
double precision,allocatable :: U(:,:) ! Potential
double precision :: pi ! pi number = 3.1415 = acos(-1.0)
integer :: n ! lattice size
integer :: i,j,k ! dummy integer variables
! ***********************************************************
! ** **
! ** **
! ** Coded By Ali Farzanehpoor **
! ** contact : ALiWM@Live.com **
! ** **
! ** **
! ***********************************************************
open(1993,file="Potential.txt")
! ------------------------------------------
n = 100
allocate(U(n,n))
allocate(rho(n,n))
U = 0 .0
Rho = 0 .0
! ------------------------------------------
! Dirichlet Boundary Condition :
U(1,:) = 10 .0
U(n,:) = 10 .0
U(:,1) = 10 .0
U(:,n) = 10 .0
Pi = acos(-1 .0)
! ------------------------------------------
! determining charge distribution(Rho) :
do i=1,n
do j=1,n
if((i .eq. j) .or. (i+j .eq. 101)) then
Rho(i,j) = 0.05
end if
end do
end do
! ------------------------------------------
write(*,*) " ******************************************************************* "
write(*,*) " Calculating Potential, Please Wait, it takes a minute or two... "
! calculating potential(U) :
Do k=1,10000
do i=2,n-1
do j=2,n-1
U(i,j) = (1 .0/4 .0)*(U(i,j+1)+U(i,j-1)+U(i+1,j)+U(i-1,j)) + Pi*Rho(i,j)
end do
end do
end do
! ------------------------------------------
! Writing Potential Matrix to the file "Potential.txt"
do k=1,n
write(1993,'(2000f25.20)') (U(k,j),j=1,n)
end do
write(*,*) " ******************************************************************* "
write(*,*) "well done"
write(*,*) " ################################################################# "
write(*,*) " Coded By Ali Farzanehpoor "
write(*,*) " Contact : Aliwm@Live.com "
write(*,*) " ################################################################# "
stop
end program Ali_Farzanehpoor
