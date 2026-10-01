PROGRAM Bahman1394
IMPLICIT NONE
DOUBLE PRECISION :: FUN1,FUN2,T,P,Y
DOUBLE PRECISION :: A,B,H
DOUBLE PRECISION :: K1, K2, K3, K4
DOUBLE PRECISION :: L1, L2, L3, L4
DOUBLE PRECISION :: W1, W2, W3
INTEGER :: I,N
!------------------------
OPEN(111,FILE="Y_YPRIME.TXT")
OPEN(222,FILE="T_Y.TXT")
OPEN(333,FILE="T_YPRIME.TXT")
!------------------------
! T:[0,150] ====> A = 0.0 ; B = 150
! H = 0.01 ====> N=(B-A)/H = 15000, THEN :
A = 0 .0
B = 150 .0
H = 1E-2
N = (B-A)/H
T = 0 .0 ! INITIAL TIME
P = 1 .0 ! VELOCITY AT (T=0)
Y = 1 .0 ! POSITION AT (T=0)
!------------------------
WRITE(111,*) Y,P
WRITE(222,*) T,Y
WRITE(333,*) T,P
!------------------------
DO I=1,N
K1 = H*FUN1(T,Y,P)
L1 = H*FUN2(T,Y,P)
!-------
W1 = T+H/2 .0 ; W2 = Y+ K1/2 .0 ; W3 = P+ L1/2 .0
K2 = H*FUN1( W1, W2, W3)
L2 = H*FUN2( W1, W2, W3)
!-------
W1 = T+H/2 .0 ; W2 = Y+ K2/2 .0 ; W3 = P+ L2/2 .0
K3 = H*FUN1( W1, W2, W3)
L3 = H*FUN2( W1, W2, W3)
!-------
W1 = T + H ; W2 = Y+ K3 ; W3 = P+ L3
K4 = H*FUN1( W1, W2, W3)
L4 = H*FUN2( W1, W2, W3)
!-------
Y = Y + (1 .0/6 .0)*( K1+2* K2+2* K3+ K4)
P = P + (1 .0/6 .0)*( L1+2* L2+2* L3+ L4)
!-------
WRITE(111,*) Y,P
WRITE(222,*) T,Y
WRITE(333,*) T,P
!------
T = T + H
END DO
!------------------------
write(*,*) " Runge-Kutta 4th order for Ordinary Differential Equation "
write(*,*) " --------------------------------------- "
write(*,*) " Programmed By Ali FarzanehPoor, Aliwm@Live.com"
write(*,*) " --------------------------------------- "
!------------------------
CLOSE(111)
CLOSE(222)
CLOSE(333)
!------------------------
STOP
END PROGRAM Bahman1394
!------------------------
FUNCTION FUN1(T,Y,P)
IMPLICIT NONE
DOUBLE PRECISION :: Y,T,P,FUN1
FUN1 = P + 0 .0*(T+Y)
END FUNCTION
!------------------------
FUNCTION FUN2(T,Y,P)
IMPLICIT NONE
DOUBLE PRECISION :: T,Y,P,FUN2
FUN2 = -0 .1*P - SIN(Y) - SIN(Y)*SIN(2*T)
END FUNCTION