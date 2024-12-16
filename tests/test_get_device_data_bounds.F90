! (C) Copyright 2022- ECMWF.
! (C) Copyright 2022- Meteo-France.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.

PROGRAM TEST_GET_DEVICE_DATA_BOUNDS
  
  USE FIELD_MODULE
  USE FIELD_FACTORY_MODULE
  USE PARKIND1
  USE FIELD_ABORT_MODULE
  IMPLICIT NONE

  CLASS(FIELD_2RB), POINTER :: F_PTR => NULL()
  REAL(KIND=JPRB), POINTER  :: PTR_CPU(:,:)
  REAL(KIND=JPRB), POINTER  :: PTR_GPU(:,:)
  LOGICAL                   :: OKAY
  INTEGER                   :: I,J
  
  CALL FIELD_NEW(F_PTR, LBOUNDS=[1,1], UBOUNDS=[128,3], PERSISTENT=.TRUE.)
  CALL F_PTR%GET_HOST_DATA_RDWR(PTR_CPU)
  PTR_CPU(:,1) = 42
  PTR_CPU(:,2) = 42
  PTR_CPU(:,3) = 37

  CALL F_PTR%GET_DEVICE_DATA_RDWR(PTR_GPU, BLK_BOUNDS=[1,2])
  OKAY=.TRUE.
  
  !$acc serial, present(PTR_GPU), copy(OKAY)
  DO I=1,128
    DO J = 1,2
      IF ( PTR_GPU(I,J) /= 42 ) THEN
        OKAY = .FALSE.
      END IF
    END DO
  END DO

  IF ( OKAY ) THEN
    DO I=1,128
      DO J = 1,2
        PTR_GPU(I,J) = 32
      END DO
    END DO
  END IF
  !$acc end serial 
   
  IF (.NOT. OKAY) THEN 
    CALL FIELD_ABORT("ERROR DATA NOT UPDATED ON DEVICE")
  END IF
  
  CALL F_PTR%SYNC_HOST_RDWR(BLK_BOUNDS=[1,2])
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

END PROGRAM TEST_GET_DEVICE_DATA_BOUNDS

