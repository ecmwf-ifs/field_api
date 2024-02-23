! (C) Copyright 2022- ECMWF.
! (C) Copyright 2022- Meteo-France.
! (C) Copyright 2023- NVIDIA
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.

PROGRAM TEST_BC
  USE FIELD_ABORT_MODULE
  USE FIELD_MODULE
  USE PARKIND1
  USE FIELD_FACTORY_MODULE
  USE FIELD_ACCESS_MODULE
  use iso_c_binding

  IMPLICIT NONE
  CLASS(FIELD_4IM), POINTER :: F4 => NULL()

  INTEGER (KIND=JPIM), PARAMETER :: NDIM = 4
  INTEGER (KIND=JPIM), PARAMETER :: DIMS (NDIM) = [10, 5, 4, 7]

  INTEGER (KIND=JPIM) :: LB (NDIM), UB (NDIM)

  INTEGER (KIND=JPIM), ALLOCATABLE, TARGET :: DATA4 (:,:,:,:)

  INTEGER (KIND=JPIM), POINTER :: Z4(:,:,:,:)

  INTEGER (KIND=JPIM), POINTER :: D4(:,:,:,:)
  INTEGER (KIND=JPIM), POINTER :: H4(:,:,:,:)

  INTEGER (KIND=JPIM) :: JDIM, I, J

  INTEGER (KIND=JPIM) :: I1, I2, I3, I4

  INTEGER (KIND=JPIM) :: K1, K2, K3, K4
  INTEGER (KIND=JPIM) :: FUNC1, FUNC2

  FUNC1 (K1, K2, K3, K4) = K1 + 100 * (K2 + 100 * (K3 + 100 * K4))
  FUNC2 (K1, K2, K3, K4) = K4 + 100 * (K3 + 100 * (K2 + 100 * K1))

  DO JDIM = 1, NDIM
    DO I = 1, DIMS (JDIM)
      DO J = I, DIMS (JDIM)

        ALLOCATE (DATA4 (DIMS (1), DIMS (2), DIMS (3), DIMS (4)))
       
        LB = 1
        UB = DIMS

        LB (JDIM) = I
        UB (JDIM) = J

        Z4 (LB (1):, LB (2):, LB (3):, LB (4):) => DATA4 (LB (1):UB (1), LB (2):UB (2), LB (3):UB (3), LB (4):UB (4))

        DO I4 = LBOUND (Z4, 4), UBOUND (Z4, 4)
          DO I3 = LBOUND (Z4, 3), UBOUND (Z4, 3)
            DO I2 = LBOUND (Z4, 2), UBOUND (Z4, 2)
              DO I1 = LBOUND (Z4, 1), UBOUND (Z4, 1)
                Z4 (I1, I2, I3, I4) = FUNC1 (I1, I2, I3, I4)
              ENDDO
            ENDDO
          ENDDO
        ENDDO

        CALL FIELD_NEW (F4, DATA=Z4, LBOUNDS=LB)

        D4 => GET_DEVICE_DATA_RDWR (F4)

!$acc serial present (D4)
        DO I4 = LBOUND (D4, 4), UBOUND (D4, 4)
          DO I3 = LBOUND (D4, 3), UBOUND (D4, 3)
            DO I2 = LBOUND (D4, 2), UBOUND (D4, 2)
              DO I1 = LBOUND (D4, 1), UBOUND (D4, 1)
                IF (D4 (I1, I2, I3, I4) /= FUNC1 (I1, I2, I3, I4)) THEN
                  PRINT *, I1, I2, I3, I4
                  PRINT *, D4 (I1, I2, I3, I4)
                  PRINT *, FUNC1 (I1, I2, I3, I4)
                  STOP 1
                ENDIF
                D4 (I1, I2, I3, I4) = FUNC2 (I1, I2, I3, I4)
              ENDDO
            ENDDO
          ENDDO
        ENDDO
!$acc end serial

        H4 => GET_HOST_DATA_RDWR (F4)

        DO I4 = LBOUND (H4, 4), UBOUND (H4, 4)
          DO I3 = LBOUND (H4, 3), UBOUND (H4, 3)
            DO I2 = LBOUND (H4, 2), UBOUND (H4, 2)
              DO I1 = LBOUND (H4, 1), UBOUND (H4, 1)
                IF (H4 (I1, I2, I3, I4) /= FUNC2 (I1, I2, I3, I4)) THEN
                  PRINT *, I1, I2, I3, I4
                  PRINT *, D4 (I1, I2, I3, I4)
                  PRINT *, FUNC2 (I1, I2, I3, I4)
                  STOP 1
                ENDIF
              ENDDO
            ENDDO
          ENDDO
        ENDDO

        CALL FIELD_DELETE (F4)

        DEALLOCATE (DATA4)

      ENDDO
    ENDDO
  ENDDO

END PROGRAM TEST_BC
