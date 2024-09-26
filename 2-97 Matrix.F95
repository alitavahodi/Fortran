PROGRAM QQQ2
IMPLICIT NONE
DOUBLE PRECISION, ALLOCATABLE :: A(:,:), B(:,:), C(:,:)
INTEGER, PARAMETER :: M = 60, N = 79, P = 5
INTEGER :: I, J, K
! -----------
OPEN(123, FILE="a.txt")
OPEN(321, FILE="b.txt")
OPEN(333, FILE="c.txt")
! -----------
ALLOCATE(A(M,N))
DO I=1, M
READ(123,*) (A(I,J), J=1, N)
END DO
! -----------
ALLOCATE(B(N,P))
DO I=1, N
READ(321,*) (B(I,J), J=1,P)
END DO
! -----------
CLOSE(123)
CLOSE(321)
! -----------
ALLOCATE(C(M,P))
C = 0.0
DO I=1, M
DO J=1, P
DO K=1,N
C(I,J) = C(I,J) + A(I,K)*B(K,J)
END DO
END DO
END DO
! -----------
DEALLOCATE(A)
DEALLOCATE(B)
! -----------
DO I=1, M
WRITE(333,"(79F14.6)") (C(I,J), J=1,P)
END DO
! -----------
DEALLOCATE(C)
CLOSE(333)
! -----------
STOP
END PROGRAM QQQ2