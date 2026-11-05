! (C) Copyright 2022- ECMWF.
! (C) Copyright 2022- Meteo-France.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.

! Black-box helpers for the PR #163 review tests.
! Preprocessor #include in the CONTAINS section of a test that USEs PARKIND1 (JPRB)
! and EC_PARKIND (JPIM).
!
! The tests use only the public FIELD API. The single criterion is:
!   data obtained through a retrieval call (GET_*_DATA, SYNC_* + GET_*_DATA,
!   GET_VIEW, the wrapped array after FIELD_DELETE) must equal the value that
!   was last written. Status flags and internal pointers are never inspected.

    SUBROUTINE STEP (CDTEXT)
      CHARACTER(LEN=*), INTENT(IN) :: CDTEXT
      WRITE (*, '(2X,A)') CDTEXT
    END SUBROUTINE STEP

    FUNCTION DATA_STR (P) RESULT (CDSTR)
      REAL(KIND=JPRB), INTENT(IN) :: P(:,:)
      CHARACTER(LEN=:), ALLOCATABLE :: CDSTR
      CHARACTER(LEN=64) :: CL

      IF (SIZE (P) == 0) THEN
        CDSTR = 'empty'
      ELSEIF (ALL (P == P(1,1))) THEN
        WRITE (CL, '(G0.4)') P(1,1)
        CDSTR = TRIM (ADJUSTL (CL))
      ELSE
        WRITE (CL, '("mixed[",G0.4,"..",G0.4,"]")') MINVAL (P), MAXVAL (P)
        CDSTR = TRIM (CL)
      ENDIF
    END FUNCTION DATA_STR

    ! Compare data obtained through the API with the value last written.
    SUBROUTINE CHECK_READ (CDWHAT, P, PEXP, KERR)
      CHARACTER(LEN=*), INTENT(IN) :: CDWHAT
      REAL(KIND=JPRB), INTENT(IN) :: P(:,:)
      REAL(KIND=JPRB), INTENT(IN) :: PEXP
      INTEGER(KIND=JPIM), INTENT(INOUT) :: KERR
      CHARACTER(LEN=64) :: CLEXP

      WRITE (CLEXP, '(G0.4)') PEXP
      IF (ANY (P /= PEXP)) THEN
        WRITE (*, '(2X,"read ",A," -> ",A,"   expected ",A,"   <== SPURIOUS DATA")') &
        &     CDWHAT, DATA_STR (P), TRIM (ADJUSTL (CLEXP))
        KERR = KERR + 1
      ELSE
        WRITE (*, '(2X,"read ",A," -> ",A,"   ok")') CDWHAT, DATA_STR (P)
      ENDIF
    END SUBROUTINE CHECK_READ

! Real kernels are needed on GPU builds: the mapped device pointers also have
! host backing storage, but ordinary host assignments do not update GPU data.
SUBROUTINE FILL_DEVICE_2D (P, PVAL)
REAL(KIND=JPRB), POINTER, INTENT(IN) :: P(:,:)
REAL(KIND=JPRB), INTENT(IN) :: PVAL
INTEGER :: I, J, N1, N2
REAL(KIND=JPRB), POINTER :: Q(:,:)
REAL(KIND=JPRB) :: ZVAL

Q => P
N1 = SIZE(P,1)
N2 = SIZE(P,2)
ZVAL = PVAL
#ifdef OMPGPU
!$omp target map(to:Q) firstprivate(ZVAL,N1,N2)
#else
!$acc serial present(Q)
#endif
DO J = 1, N2
  DO I = 1, N1
    Q(I,J) = ZVAL
  ENDDO
ENDDO
#ifdef OMPGPU
!$omp end target
#else
!$acc end serial
#endif
END SUBROUTINE FILL_DEVICE_2D

SUBROUTINE FILL_DEVICE_3D (P, PVAL)
REAL(KIND=JPRB), POINTER, INTENT(IN) :: P(:,:,:)
REAL(KIND=JPRB), INTENT(IN) :: PVAL
INTEGER :: I, J, K, N1, N2, N3
REAL(KIND=JPRB), POINTER :: Q(:,:,:)
REAL(KIND=JPRB) :: ZVAL

Q => P
N1 = SIZE(P,1)
N2 = SIZE(P,2)
N3 = SIZE(P,3)
ZVAL = PVAL
#ifdef OMPGPU
!$omp target map(to:Q) firstprivate(ZVAL,N1,N2,N3)
#else
!$acc serial present(Q)
#endif
DO K = 1, N3
  DO J = 1, N2
    DO I = 1, N1
      Q(I,J,K) = ZVAL
    ENDDO
  ENDDO
ENDDO
#ifdef OMPGPU
!$omp end target
#else
!$acc end serial
#endif
END SUBROUTINE FILL_DEVICE_3D

! Snapshot the returned device pointer without another FIELD API call, so that
! checking the data does not itself change field freshness or synchronize it.
SUBROUTINE CHECK_DEVICE_READ (CDWHAT, P, PEXP, KERR)
CHARACTER(LEN=*), INTENT(IN) :: CDWHAT
REAL(KIND=JPRB), POINTER, INTENT(IN) :: P(:,:)
REAL(KIND=JPRB), INTENT(IN) :: PEXP
INTEGER(KIND=JPIM), INTENT(INOUT) :: KERR
REAL(KIND=JPRB) :: Z(SIZE(P,1),SIZE(P,2))
INTEGER :: I, J, N1, N2
REAL(KIND=JPRB), POINTER :: Q(:,:)

Q => P
N1 = SIZE(P,1)
N2 = SIZE(P,2)
#ifdef OMPGPU
!$omp target map(to:Q) map(from:Z) firstprivate(N1,N2)
#else
!$acc serial present(Q) copyout(Z)
#endif
DO J = 1, N2
  DO I = 1, N1
    Z(I,J) = Q(I,J)
  ENDDO
ENDDO
#ifdef OMPGPU
!$omp end target
#else
!$acc end serial
#endif
CALL CHECK_READ (CDWHAT, Z, PEXP, KERR)
END SUBROUTINE CHECK_DEVICE_READ
