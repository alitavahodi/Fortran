PROGRAM Calculate_Mean_and_Standard_Deviation
IMPLICIT NONE

INTEGER :: I, N
REAL :: SUM, SUM_SQ, AVERAGE, STD
REAL, ALLOCATABLE :: A(:)

WRITE(*,*) 'Enter N:'
READ(*,*) N

ALLOCATE(A(N))

OPEN(12, FILE='random.txt', STATUS='OLD', ACTION='READ')

DO I = 1, N
    READ(12,*) A(I)
END DO

CLOSE(12)

! Calculate the arithmetic mean
SUM = 0.0
DO I = 1, N
    SUM = SUM + A(I)
END DO

AVERAGE = SUM / REAL(N)

WRITE(*,*) 'Average = ', AVERAGE

! Calculate the sample standard deviation
SUM_SQ = 0.0
DO I = 1, N
    SUM_SQ = SUM_SQ + (A(I) - AVERAGE)**2
END DO

STD = SQRT(SUM_SQ / REAL(N - 1))

WRITE(*,*) 'Standard Deviation = ', STD

DEALLOCATE(A)

STOP
END PROGRAM Calculate_Mean_and_Standard_Deviation
