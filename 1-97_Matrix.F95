PROGRAM QQQ1
IMPLICIT NONE
DOUBLE PRECISION :: NUMBERS(100)
DOUBLE PRECISION :: TEMP
INTEGER :: I, J
INTEGER, PARAMETER :: N=100
! -----------------
OPEN(544,FILE="sort.txt")
! -----------------
CALL RANDOM_SEED()
! -----------------
DO I=1, N
CALL RANDOM_NUMBER(NUMBERS(I))
END DO
! -----------------
TEMP = 0.0
DO I=1, N
DO J=1, N
IF(NUMBERS(I) .GT. NUMBERS(J)) THEN
TEMP = NUMBERS(J)
NUMBERS(J) = NUMBERS(I)
NUMBERS(I) = TEMP
END IF
END DO
END DO
! -----------------
DO I=1, N
WRITE(544,*) NUMBERS(I) ! IN FILE
END DO
! -----------------
CLOSE(544)
! -----------------
STOP
END PROGRAM QQQ1