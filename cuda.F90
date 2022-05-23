PROGRAM CUDA

USE CUDAFOR
USE OPENACC

IMPLICIT NONE

INTEGER, PARAMETER :: N = 100

REAL*8, DEVICE :: DEV (N)
REAL*8 :: HST (N)
REAL*8, POINTER :: DEV1 (:)

INTEGER :: I
INTEGER :: ISIZE

DO I = 1, N
  HST (I) = REAL (I, 8)
ENDDO

ISIZE = N * KIND (DEV)

CALL ACC_MEMCPY_TO_DEVICE (DEV (:), HST (:), ISIZE)

ALLOCATE (DEV1, MOLD=HST)
!$acc enter data create (DEV1)

!$acc host_data use_device (DEV1)
CALL ACC_MEMCPY_TO_DEVICE (DEV1 (:), HST (:), ISIZE)
!$acc end host_data

!$acc serial present (DEV1)
DO I = 1, N
  PRINT *, I, DEV1 (I)
ENDDO
!$acc end serial



END
