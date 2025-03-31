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
  USE FIELD_CONSTANTS_MODULE
  IMPLICIT NONE

  CLASS(FIELD_3RB), POINTER :: F_PTR => NULL()
  REAL(KIND=JPRB), POINTER  :: PTR_CPU(:,:,:)
  REAL(KIND=JPRB), POINTER  :: PTR_GPU(:,:,:)
  INTEGER                   :: I,J,K
  INTEGER, PARAMETER        :: NQUEUES = 3
  INTEGER, PARAMETER        :: INNER_RANK = 32
  INTEGER, PARAMETER        :: FINAL_RANK = 67
  LOGICAL                   :: OKAY(NQUEUES)
  LOGICAL                   :: OKAY_SCALAR
  INTEGER                   :: QUEUE
  INTEGER                   :: BLK_SIZE
  INTEGER                   :: BUFFER_SIZE
  INTEGER                   :: BLK_IDX
  INTEGER                   :: BLK_START, BLK_END
  INTEGER                   :: BLK_COUNT
  INTEGER                   :: OFFSET
  INTEGER                   :: BLK_BOUNDS(2)

  OKAY = .TRUE.
  OKAY_SCALAR = .TRUE.

  CALL FIELD_NEW(F_PTR, LBOUNDS=[1,1,1], UBOUNDS=[INNER_RANK,INNER_RANK,FINAL_RANK], PERSISTENT=.TRUE.)
  CALL F_PTR%GET_HOST_DATA_RDWR(PTR_CPU)

  DO K=1,FINAL_RANK
    PTR_CPU(:,:,K)=K
  END DO

  BUFFER_SIZE = 12
  BLK_COUNT=(FINAL_RANK+BUFFER_SIZE-1)/BUFFER_SIZE
  CALL F_PTR%CREATE_DEVICE_DATA(BLK_BOUNDS=[1,NQUEUES*BUFFER_SIZE])

  !$acc enter data copyin(OKAY)

  DO BLK_IDX = 0, BLK_COUNT-1
    BLK_START=BLK_IDX*BUFFER_SIZE+1
    BLK_END= MIN((BLK_IDX+1)*BUFFER_SIZE, FINAL_RANK)
    BLK_BOUNDS(1) = BLK_START
    BLK_BOUNDS(2) = BLK_END
    BLK_SIZE = BLK_BOUNDS(2) - BLK_BOUNDS(1) + 1
    QUEUE = MODULO(BLK_IDX,NQUEUES)+1
    OFFSET = (QUEUE-1)*BUFFER_SIZE

    CALL F_PTR%GET_DEVICE_DATA_FORCE(PTR_GPU, BLK_BOUNDS=BLK_BOUNDS, QUEUE=QUEUE, OFFSET=OFFSET)

    !$acc parallel loop present(PTR_GPU) async(QUEUE)
    DO K = BLK_START, BLK_END
      DO J = 1,INNER_RANK
        DO I=1,INNER_RANK
          IF ( PTR_GPU(I,J,K) /= K) THEN
            OKAY(QUEUE) = .FALSE.
          END IF
          PTR_GPU(I,J,K) = 100 + K
        END DO
      END DO
    END DO
    !$acc end parallel loop
    CALL F_PTR%SYNC_HOST_FORCE(BLK_BOUNDS=BLK_BOUNDS, QUEUE=QUEUE, OFFSET=OFFSET)

  END DO

  !$acc wait
  !$acc update host(OKAY)
  !$acc exit data delete(OKAY)

  DO QUEUE = 1,NQUEUES
    IF (.NOT. OKAY(QUEUE) ) THEN
      PRINT *, "FAILURE IN QUEUE:", QUEUE
      CALL FIELD_ABORT("ERROR DATA NOT UPDATED ON DEVICE (ASYNC COPY)")
    END IF
  END DO

  DO K=1,FINAL_RANK
    DO I=1,INNER_RANK
      DO J = 1,INNER_RANK
        IF ( PTR_CPU(I,J,K) /= 100 + K ) THEN
          OKAY_SCALAR = .FALSE.
          print *, "failure in I J K:", I, J, K, "PTR_CPU(I,J,K) =", PTR_CPU(I,J,K)
        END IF
      END DO
    END DO
  END DO
  IF (.NOT. OKAY_SCALAR ) THEN
    CALL FIELD_ABORT("ERROR DATA NOT UPDATED ON HOST (ASYNC COPY)")
  END  IF


  BUFFER_SIZE = (FINAL_RANK-1)/NQUEUES + 1
  DO QUEUE = 1,NQUEUES
    IF ( QUEUE == NQUEUES ) THEN
      BLK_SIZE = FINAL_RANK - BUFFER_SIZE*(NQUEUES-1)
    ELSE
      BLK_SIZE = BUFFER_SIZE
    ENDIF
    DO J = 1,BLK_SIZE
      PTR_CPU(:,:,(QUEUE-1)*BUFFER_SIZE + J) = QUEUE*111
      OKAY(QUEUE) = .TRUE.
    END DO
  END DO


  !$acc enter data copyin(OKAY)

  CALL F_PTR%CREATE_DEVICE_DATA(BLK_BOUNDS=[1,FINAL_RANK])

  DO QUEUE=1,NQUEUES
    ! we need to retrieve the correct ptr for each queue, this will not move memory,
    ! but only point ptr_gpu to the correct chunk on the gpu.
    IF ( QUEUE == NQUEUES ) THEN
      BLK_SIZE = FINAL_RANK - BUFFER_SIZE*(NQUEUES-1)
    ELSE
      BLK_SIZE = BUFFER_SIZE
    END IF
    BLK_BOUNDS(1) = (QUEUE-1)*BUFFER_SIZE +1
    BLK_BOUNDS(2) = BLK_BOUNDS(1) + BLK_SIZE -1

    OFFSET = BLK_BOUNDS(1)-1

    CALL F_PTR%GET_DEVICE_DATA_FORCE(PTR_GPU, QUEUE, BLK_BOUNDS, OFFSET)

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

    CALL F_PTR%SYNC_HOST_FORCE(QUEUE=QUEUE, BLK_BOUNDS=BLK_BOUNDS, OFFSET=OFFSET)
  END DO

  !$acc wait
  !$acc exit data copyout(OKAY)

  DO QUEUE = 1,NQUEUES
    IF (.NOT. OKAY(QUEUE) ) THEN
      CALL FIELD_ABORT("ERROR DATA NOT UPDATED ON DEVICE (ASYNC COPY)")
    END IF

    IF ( QUEUE == NQUEUES ) THEN
      BLK_SIZE = FINAL_RANK - BUFFER_SIZE*(NQUEUES-1)
    ELSE
      BLK_SIZE = BUFFER_SIZE
    ENDIF

    DO I=1,INNER_RANK
      DO J = 1,INNER_RANK
        DO K =1,BLK_SIZE
          IF ( PTR_CPU(I,J,(QUEUE-1)*BUFFER_SIZE+K) /= QUEUE ) THEN
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
  CALL  F_PTR%SET_STATUS(NHSTFRESH)

  DO QUEUE = 1,NQUEUES
    IF ( QUEUE == NQUEUES ) THEN
      BLK_SIZE = FINAL_RANK - BUFFER_SIZE*(NQUEUES-1)
    ELSE
      BLK_SIZE = BUFFER_SIZE
    ENDIF

    DO J = 1,BLK_SIZE
      PTR_CPU(:,:,(QUEUE-1)*BUFFER_SIZE + J) = QUEUE*111
      OKAY(QUEUE) = .TRUE.
    END DO
  END DO

  CALL F_PTR%GET_DEVICE_DATA_RDWR(PTR_GPU)

  !$acc serial copyin(QUEUE), copy(OKAY), present(PTR_GPU)
  DO QUEUE = 1,NQUEUES
    IF ( QUEUE == NQUEUES ) THEN
      BLK_SIZE = FINAL_RANK - BUFFER_SIZE*(NQUEUES-1)
    ELSE
      BLK_SIZE = BUFFER_SIZE
    ENDIF
    DO I=1,INNER_RANK
      DO J = 1,INNER_RANK
        DO K =1,BLK_SIZE
          IF ( PTR_GPU(I,J,(QUEUE-1)*BUFFER_SIZE+K) /= QUEUE*111 ) THEN
            OKAY(QUEUE) = .FALSE.
          ENDIF
          PTR_GPU(I,J,(QUEUE-1)*BUFFER_SIZE+K) = QUEUE
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

    IF ( QUEUE == NQUEUES ) THEN
      BLK_SIZE = FINAL_RANK - BUFFER_SIZE*(NQUEUES-1)
    ELSE
      BLK_SIZE = BUFFER_SIZE
    ENDIF

    DO I=1,INNER_RANK
      DO J = 1,INNER_RANK
        DO K =1,BLK_SIZE
          IF ( PTR_CPU(I,J,(QUEUE-1)*BUFFER_SIZE+K) /= QUEUE ) THEN
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
