! (C) Copyright 2022- ECMWF.
! (C) Copyright 2022- Meteo-France.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.

! This is a simple test to verify the async functionality of the FORCE data
! transfer methods. The kernel in this test is not wrapped inside any acc data
! region, as opposed to in the test get_device_data_multi_blk_async. The
! reason for this is that data regions may be blocking, depending on compiler
! version, which leads to synchronous execution. This test executes
! asynchronously when compiled with NVHPC compiler versions 22.11 and 24.5.

PROGRAM TEST_FORCE_ASYNC

  USE FIELD_MODULE
  USE FIELD_FACTORY_MODULE
  USE PARKIND1
  USE FIELD_ABORT_MODULE
  USE FIELD_DEFAULTS_MODULE
  USE FIELD_ASYNC_MODULE, ONLY: WAIT_FOR_ASYNC_QUEUE

  IMPLICIT NONE

  CLASS(FIELD_3RB), POINTER :: F_PTR => NULL()
  REAL(KIND=JPRB), POINTER  :: PTR_CPU(:,:,:)
  REAL(KIND=JPRB), POINTER  :: PTR_GPU(:,:,:)
  INTEGER                   :: I,J,K
  INTEGER, PARAMETER        :: NQUEUES = 3
  INTEGER, PARAMETER        :: INNER_RANK = 64
  INTEGER, PARAMETER        :: FINAL_RANK = 37
  LOGICAL                   :: OKAY
  INTEGER                   :: QUEUE
  INTEGER                   :: BLK_SIZE
  INTEGER                   :: BUFFER_SIZE
  INTEGER                   :: BLK_IDX
  INTEGER                   :: BLK_START, BLK_END
  INTEGER                   :: BLK_COUNT
  INTEGER                   :: OFFSET
  INTEGER                   :: BLK_BOUNDS(2)

  INIT_PINNED_VALUE = .TRUE.
  OKAY = .TRUE.
  OKAY = .TRUE.

  CALL FIELD_NEW(F_PTR, LBOUNDS=[1,1,1], UBOUNDS=[INNER_RANK,INNER_RANK,FINAL_RANK], PERSISTENT=.TRUE.)
  CALL F_PTR%GET_HOST_DATA_RDWR(PTR_CPU)
  DO K=1,FINAL_RANK
    PTR_CPU(:,:,K)=K
  END DO

  ! ALLOCATE DEVICE "BLOCK BUFFERS"
  BUFFER_SIZE = 12
  BLK_COUNT=(FINAL_RANK+BUFFER_SIZE-1)/BUFFER_SIZE
  CALL F_PTR%CREATE_DEVICE_DATA(BLK_BOUNDS=[1,NQUEUES*BUFFER_SIZE])

  ! LOOP OVER THE BLOCKS AND COPY DATA TO HOST
  DO BLK_IDX = 0, BLK_COUNT-1
    BLK_START=BLK_IDX*BUFFER_SIZE+1
    BLK_END= MIN((BLK_IDX+1)*BUFFER_SIZE, FINAL_RANK)
    BLK_BOUNDS(1) = BLK_START
    BLK_BOUNDS(2) = BLK_END
    BLK_SIZE = BLK_BOUNDS(2) - BLK_BOUNDS(1) + 1
    QUEUE = MODULO(BLK_IDX, NQUEUES)+1
    OFFSET = (QUEUE-1)*BUFFER_SIZE

    CALL F_PTR%GET_DEVICE_DATA_FORCE(PTR_GPU, BLK_BOUNDS=BLK_BOUNDS, QUEUE=QUEUE, OFFSET=OFFSET)

    !$acc kernels loop present(ptr_gpu) async(queue)
    DO K = BLK_START, BLK_END
      DO J = 1,INNER_RANK
        DO I=1,INNER_RANK
          PTR_GPU(I,J,K) = 100 + K
        END DO
      END DO
    END DO
    !$acc end kernels

    CALL F_PTR%SYNC_HOST_FORCE(BLK_BOUNDS=BLK_BOUNDS, QUEUE=QUEUE, OFFSET=OFFSET)
  END DO

  DO QUEUE=1,NQUEUES
    CALL WAIT_FOR_ASYNC_QUEUE(QUEUE)
  END DO

  DO K=1,FINAL_RANK
    DO J = 1,INNER_RANK
      DO I=1,INNER_RANK
        IF ( PTR_CPU(I,J,K) /= 100 + K ) THEN
          OKAY = .FALSE.
          CALL FIELD_ABORT("ERROR DATA NOT UPDATED ON HOST (ASYNC COPY)")
        END IF
      END DO
    END DO
  END DO

END PROGRAM TEST_FORCE_ASYNC
