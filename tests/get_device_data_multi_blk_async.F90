! (C) Copyright 2022- ECMWF.
! (C) Copyright 2022- Meteo-France.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.

PROGRAM TEST_GET_DEVICE_DATA_MULTI_BLK_ASYNC
  
  USE FIELD_MODULE
  USE FIELD_FACTORY_MODULE
  USE PARKIND1
  USE FIELD_ABORT_MODULE
  USE FIELD_DEFAULTS_MODULE, ONLY: INIT_QUEUES, DELETE_QUEUES
  IMPLICIT NONE

  CLASS(FIELD_3RB), POINTER :: F_PTR => NULL()
  REAL(KIND=JPRB), POINTER  :: PTR_CPU(:,:,:)
  REAL(KIND=JPRB), POINTER  :: PTR_GPU(:,:,:)
  INTEGER                   :: I,J,K
  INTEGER, PARAMETER        :: NQUEUES = 3
  INTEGER, PARAMETER        :: INNER_RANK = 32
  INTEGER, PARAMETER        :: FINAL_RANK = 12
  LOGICAL                   :: OKAY(NQUEUES)
  INTEGER                   :: QUEUE
  INTEGER                   :: BLK_SIZE = 4
  INTEGER                   :: BLK_IDX, OFFSET
  INTEGER                   :: BLK_BOUNDS(2)
  
  CALL FIELD_NEW(F_PTR, LBOUNDS=[1,1,1], UBOUNDS=[INNER_RANK,INNER_RANK,FINAL_RANK], PERSISTENT=.TRUE.)
  CALL F_PTR%GET_HOST_DATA_RDWR(PTR_CPU)

  DO QUEUE = 1,NQUEUES
    DO J = 1,BLK_SIZE
      PTR_CPU(:,:,(QUEUE-1)*BLK_SIZE + J) = QUEUE*111
      OKAY(QUEUE) = .TRUE.
    END DO
  END DO


  !$acc enter data copyin(OKAY)

  ! this will initiate async memory transfer mode on the F_PTR field
  CALL INIT_QUEUES(NQUEUES=NQUEUES)
  ! this will launch NQUEUE asynchronous memcopies
  CALL F_PTR%SYNC_DEVICE_RDWR()

  DO QUEUE=1,NQUEUES
    ! we need to retrieve the correct ptr for each queue, this will not move memory,
    ! but only point ptr_gpu to the correct chunk on the gpu.
    CALL F_PTR%GET_DEVICE_DATA_RDWR(PTR_GPU, QUEUE)
    !$acc serial copyin(QUEUE), present(PTR_GPU), present(OKAY), async(QUEUE)
    DO I=1,INNER_RANK
      DO J = 1,INNER_RANK
        DO K = LBOUND(PTR_GPU,3),UBOUND(PTR_GPU,3)
          IF ( PTR_GPU(I,J,K) /= QUEUE*111 ) THEN
            OKAY(QUEUE) = .FALSE.
          END IF
          PTR_GPU(I,J,K) = QUEUE
        END DO
      END DO
    END DO
    !$acc end serial 
  END DO

  CALL F_PTR%SYNC_HOST_RDWR()
  !$acc wait
  !$acc exit data copyout(OKAY)

  DO QUEUE = 1,NQUEUES
    IF (.NOT. OKAY(QUEUE) ) THEN
      CALL FIELD_ABORT("ERROR DATA NOT UPDATED ON DEVICE (ASYNC COPY)")
    END IF
      DO I=1,INNER_RANK
        DO J = 1,INNER_RANK
          DO K =1,BLK_SIZE
            IF ( PTR_CPU(I,J,(QUEUE-1)*BLK_SIZE+K) /= QUEUE ) THEN
              OKAY(QUEUE) = .FALSE.
            END IF
          END DO
        END DO
      END DO
    IF (.NOT. OKAY(QUEUE) ) THEN
      CALL FIELD_ABORT("ERROR DATA NOT UPDATED ON HOST (ASYNC COPY)")
    END  IF
  END DO


  ! Test that normal copies works after returning from "async queue-mode"
  CALL DELETE_QUEUES()

  DO QUEUE = 1,NQUEUES
    DO J = 1,BLK_SIZE
      PTR_CPU(:,:,(QUEUE-1)*BLK_SIZE + J) = QUEUE*111
      OKAY(QUEUE) = .TRUE.
    END DO
  END DO

  CALL F_PTR%GET_DEVICE_DATA_RDWR(PTR_GPU)

  !$acc serial copyin(QUEUE), copy(OKAY), present(PTR_GPU)
  DO QUEUE = 1,NQUEUES
      DO I=1,INNER_RANK
        DO J = 1,INNER_RANK
          DO K =1,BLK_SIZE
            IF ( PTR_GPU(I,J,(QUEUE-1)*BLK_SIZE+K) /= QUEUE*111 ) THEN
              OKAY(QUEUE) = .FALSE.
            ENDIF
            PTR_GPU(I,J,(QUEUE-1)*BLK_SIZE+K) = QUEUE
          END DO
        END DO
      END DO
  END DO
  !$acc end serial

  CALL F_PTR%SYNC_HOST_RDWR()

  DO QUEUE = 1,NQUEUES
    IF (.NOT. OKAY(QUEUE) ) THEN
      CALL FIELD_ABORT("ERROR DATA NOT UPDATED ON DEVICE")
    END IF
      DO I=1,INNER_RANK
        DO J = 1,INNER_RANK
          DO K =1,BLK_SIZE
            IF ( PTR_CPU(I,J,(QUEUE-1)*BLK_SIZE+K) /= QUEUE ) THEN
              OKAY(QUEUE) = .FALSE.
            END IF
          END DO
        END DO
      END DO
    IF (.NOT. OKAY(QUEUE) ) THEN
      CALL FIELD_ABORT("ERROR DATA NOT UPDATED ON HOST")
    END  IF
  END DO

END PROGRAM TEST_GET_DEVICE_DATA_MULTI_BLK_ASYNC

