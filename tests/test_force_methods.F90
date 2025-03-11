! (C) Copyright 2022- ECMWF.
! (C) Copyright 2022- Meteo-France.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.

PROGRAM TEST_FORCE_METHODS

  USE FIELD_MODULE
  USE FIELD_FACTORY_MODULE
  USE FIELD_CONSTANTS_MODULE
  USE PARKIND1
  USE FIELD_ABORT_MODULE
  IMPLICIT NONE

  CLASS(FIELD_2RB), POINTER :: F_PTR => NULL()
  REAL(KIND=JPRB), POINTER  :: PTR_CPU(:,:) => NULL()
  REAL(KIND=JPRB), POINTER  :: PTR_GPU(:,:) => NULL()
  REAL(KIND=JPRB), POINTER  :: PTR_GPU2(:,:) => NULL()
  LOGICAL                   :: OKAY
  INTEGER                   :: I,J

  CALL FIELD_NEW(F_PTR, LBOUNDS=[1,1], UBOUNDS=[128,3], PERSISTENT=.TRUE.)
  CALL F_PTR%GET_HOST_DATA_RDWR(PTR_CPU)
  PTR_CPU(:,1) = 42
  PTR_CPU(:,2) = 42
  PTR_CPU(:,3) = 37

  CALL F_PTR%GET_DEVICE_DATA_FORCE(PTR_GPU)
  OKAY=.TRUE.

  !$acc serial, present(PTR_GPU), copy(OKAY)
  DO I=1,128
    DO J = 1,2
      IF ( PTR_GPU(I,J) /= 42 ) THEN
        OKAY = .FALSE.
      END IF
      PTR_GPU(I,J) = 32
    END DO
  END DO
  !$acc end serial

  IF (.NOT. OKAY) THEN
    CALL FIELD_ABORT("ERROR DATA NOT UPDATED ON DEVICE")
  END IF

  CALL F_PTR%SYNC_HOST_FORCE()
  IF ( F_PTR%GET_STATUS() /= UNDEFINED ) THEN
    CALL FIELD_ABORT("ERROR FIELD STATUS SHOULD BE UNDEFINED")
  END IF

  DO I=1,128
    DO J = 1,2
      IF ( PTR_CPU(I,J) /= 32 ) THEN
        OKAY =.FALSE.
      END IF
    END DO
  END DO

  IF (.NOT. OKAY) THEN
    CALL FIELD_ABORT("ERROR HOST DATA NOT UPDATED BY SYNC_HOST_RDWR")
  END  IF

  DO I=1,128
    IF ( PTR_CPU(I,3) /= 37 ) THEN
      OKAY = .FALSE.
    END IF
  END DO

  IF (.NOT. OKAY) THEN
    CALL FIELD_ABORT("THIRD COLUMN SHOULD NOT HAVE BEEN UPDATED")
  END  IF


  PTR_CPU(:,1) = 42
  PTR_CPU(:,2) = 42
  ! Status should be device fresh and no data movement should be triggered
  CALL F_PTR%FORCE_DEVICE_FRESH()

  IF ( F_PTR%GET_STATUS() == UNDEFINED ) THEN
    CALL FIELD_ABORT("ERROR FIELD STATUS SHOULD NOT BE UNDEFINED")
  END IF
  IF ( IAND(F_PTR%GET_STATUS(), NDEVFRESH) == 0) THEN
    CALL FIELD_ABORT("ERROR FIELD SHOULD BE FRESH ON DEVICE")
  END IF

  CALL F_PTR%GET_DEVICE_DATA_RDONLY(PTR_GPU)

  !$acc serial, present(PTR_GPU), copy(OKAY)
  DO I=1,128
    DO J = 1,2
      IF ( PTR_GPU(I,J) /= 32 ) THEN
        OKAY = .FALSE.
      END IF
      PTR_GPU(I,J) = 45
    END DO
  END DO
  !$acc end serial

  IF (.NOT. OKAY) THEN
    CALL FIELD_ABORT("ERROR GPU DATA SHOULD NOT HAVE BEEN UPDATED")
  END  IF

  CALL F_PTR%SYNC_HOST_RDWR()
  DO I=1,128
    DO J = 1,2
      IF ( PTR_CPU(I,J) /= 45 ) THEN
        OKAY =.FALSE.
      END IF
    END DO
  END DO

  IF (.NOT. OKAY) THEN
    CALL FIELD_ABORT("ERROR CPU DATA SHOULD HAVE BEEN UPDATED")
  END  IF


  CALL F_PTR%DELETE_DEVICE_DATA()
  CALL F_PTR%GET_DEVICE_DATA_FORCE(PTR_GPU)
  !$acc serial, present(PTR_GPU), copy(OKAY)
  DO I=1,128
    DO J = 1,2
      IF ( PTR_GPU(I,J) /= 45 ) THEN
        OKAY = .FALSE.
      END IF
      PTR_GPU(I,J) = 46
    END DO
  END DO
  !$acc end serial
  IF (.NOT. OKAY) THEN
    CALL FIELD_ABORT("ERROR GPU DATA NOT UPDATED")
  END  IF


  CALL F_PTR%DELETE_DEVICE_DATA()
  ! No copy should be triggered after deleting device data
  CALL F_PTR%GET_HOST_DATA_FORCE(PTR_CPU)

  DO I=1,128
    DO J = 1,2
      IF ( PTR_CPU(I,J) /= 45 ) THEN
        OKAY =.FALSE.
      END IF
    END DO
  END DO

  IF (.NOT. OKAY) THEN
    CALL FIELD_ABORT("ERROR CPU DATA SHOULD HAVE BEEN UPDATED")
  END  IF


END PROGRAM TEST_FORCE_METHODS
