! (C) Copyright 2022- ECMWF.
! (C) Copyright 2022- Meteo-France.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.

PROGRAM INIT_OWNER_FIELD_BUFFER_GPU
        ! TEST IF OWNER IS REALLY ALLOCATING THE DATA
        ! WHEN PERSITENT IS SET TO TRUE OR IS NOT GIVEN IN ARGUMENT,
        ! THEN THE LAST DIM OF THE FIELD IS THE NUMBER OF OPENMP THREADS

        USE FIELD_MODULE
        USE FIELD_BUFFER_MODULE
        USE PARKIND1

        IMPLICIT NONE

        TYPE(FIELD_3RB_BUFFER) :: BUFFER
        TYPE(FIELD_2RB_PTR), ALLOCATABLE :: FIELDS(:)

        REAL(KIND=JPRB), POINTER :: BUFFER_CPU(:,:,:)
        REAL(KIND=JPRB), POINTER :: BUFFER_GPU(:,:,:)
        REAL(KIND=JPRB), POINTER :: FIELD_PTR(:,:)

        LOGICAL :: RES
        INTEGER(KIND=JPIM) :: NFIELDS, IFIELD, I, J

        NFIELDS = 3
        ALLOCATE(FIELDS(NFIELDS))
        
        CALL BUFFER%INIT(NFIELDS, FIELDS, LBOUNDS=[10,1,1], UBOUNDS=[21,NFIELDS,10], PERSISTENT=.TRUE.)

        CALL BUFFER%GET_HOST_DATA_RDWR(BUFFER_CPU)
        BUFFER_CPU = 42

        CALL FIELDS(1)%PTR%GET_HOST_DATA_RDWR(FIELD_PTR)
        IF(.NOT. ALL(FIELD_PTR == 42)) ERROR STOP
        CALL FIELDS(2)%PTR%GET_HOST_DATA_RDWR(FIELD_PTR)
        IF(.NOT. ALL(FIELD_PTR == 42)) ERROR STOP
        CALL FIELDS(3)%PTR%GET_HOST_DATA_RDWR(FIELD_PTR)
        IF(.NOT. ALL(FIELD_PTR == 42)) ERROR STOP

        CALL BUFFER%GET_DEVICE_DATA_RDWR(BUFFER_GPU)
        RES=.TRUE.
#ifdef _CUDA
        !$ACC SERIAL DEVICEPTR (BUFFER_GPU) COPY(RES)
#else
        !$ACC SERIAL PRESENT (BUFFER_GPU) COPY(RES)
#endif
        DO J=1,10
          DO IFIELD=1,NFIELDS
            DO I=10,21
              IF(BUFFER_GPU(I,IFIELD,J) /= 42) THEN
                      RES = .FALSE.
              END IF
            END DO
          END DO
        END DO

        !$ACC END SERIAL
        IF(.NOT. RES) ERROR STOP

        CALL FIELDS(1)%PTR%GET_DEVICE_DATA_RDWR(FIELD_PTR)
#ifdef _CUDA
        !$ACC SERIAL DEVICEPTR (FIELD_PTR)
#else
        !$ACC SERIAL PRESENT (FIELD_PTR)
#endif
        DO J=1,10
          DO I=10,21
             FIELD_PTR(I,J) = 1
          END DO
        END DO
        !$ACC END SERIAL

        CALL FIELDS(2)%PTR%GET_DEVICE_DATA_RDWR(FIELD_PTR)
#ifdef _CUDA
        !$ACC SERIAL DEVICEPTR (FIELD_PTR)
#else
        !$ACC SERIAL PRESENT (FIELD_PTR)
#endif
        DO J=1,10
          DO I=10,21
             FIELD_PTR(I,J) = 2
          END DO
        END DO
        !$ACC END SERIAL

        CALL FIELDS(3)%PTR%GET_DEVICE_DATA_RDWR(FIELD_PTR)
#ifdef _CUDA
        !$ACC SERIAL DEVICEPTR (FIELD_PTR)
#else
        !$ACC SERIAL PRESENT (FIELD_PTR)
#endif
        DO J=1,10
          DO I=10,21
             FIELD_PTR(I,J) = 3
          END DO
        END DO
        !$ACC END SERIAL

        CALL BUFFER%SYNC_HOST_RDWR()
        IF(.NOT. ALL(BUFFER_CPU(:,1,:) == 1))THEN
            ERROR STOP
        ENDIF
        IF(.NOT. ALL(BUFFER_CPU(:,2,:) == 2))THEN
            ERROR STOP
        ENDIF
        IF(.NOT. ALL(BUFFER_CPU(:,3,:) == 3))THEN
            ERROR STOP
        ENDIF

        CALL FIELDS(1)%PTR%GET_HOST_DATA_RDWR(FIELD_PTR)
        IF(.NOT. ALL(FIELD_PTR == 1)) ERROR STOP
        CALL FIELDS(2)%PTR%GET_HOST_DATA_RDWR(FIELD_PTR)
        IF(.NOT. ALL(FIELD_PTR == 2)) ERROR STOP
        CALL FIELDS(3)%PTR%GET_HOST_DATA_RDWR(FIELD_PTR)
        IF(.NOT. ALL(FIELD_PTR == 3)) ERROR STOP

        CALL BUFFER%FINAL()

        DEALLOCATE(FIELDS)
END PROGRAM INIT_OWNER_FIELD_BUFFER_GPU
