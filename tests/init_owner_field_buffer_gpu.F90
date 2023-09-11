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

        REAL(KIND=JPRB), POINTER :: PTR_CPU(:,:,:)
        REAL(KIND=JPRB), POINTER :: PTR_GPU(:,:,:)
        TYPE(FIELD_3RB_VIEW), ALLOCATABLE :: FIELD_PTRS_GPU(:)

        LOGICAL :: RES
        INTEGER(KIND=JPIM) :: NFIELDS, IFIELD, I, J

        NFIELDS = 3
        ALLOCATE(FIELDS(NFIELDS))
        ALLOCATE(FIELD_PTRS_GPU(NFIELDS))
        
        CALL BUFFER%INIT(NFIELDS, FIELDS, LBOUNDS=[10,1,1], UBOUNDS=[21,NFIELDS,10])

        CALL BUFFER%GET_HOST_DATA_RDWR(PTR_CPU)
        CALL BUFFER%GET_DEVICE_DATA_RDWR(PTR_GPU, FIELDS=FIELD_PTRS_GPU)
        PTR_CPU = 42

        RES=.TRUE.
#ifdef _CUDA
        !$ACC SERIAL DEVICEPTR (PTR_GPU,FIELD_PTRS_GPU) COPY(RES)
#else
        !$ACC SERIAL PRESENT (PTR_GPU,FIELD_PTRS_GPU(1)%P,FIELD_PTRS_GPU(2)%P,FIELD_PTRS_GPU(3)%P) COPY(RES)
#endif
        DO J=1,11
          DO IFIELD=1,NFIELDS
            DO I=10,21
              IF(PTR_GPU(I,IFIELD,J) /= 42) THEN
                      RES = .FALSE.
              END IF
            END DO
          END DO
        END DO

        DO J=1,11
          DO I=10,21
             FIELD_PTRS_GPU(1)%P(I,J) = 1
             FIELD_PTRS_GPU(2)%P(I,J) = 2
             FIELD_PTRS_GPU(3)%P(I,J) = 3
          END DO
        END DO
        !$ACC END SERIAL

        CALL BUFFER%SYNC_HOST_RDWR()
        IF(.NOT. ALL(PTR_CPU(:,1,:) == 1))THEN
            ERROR STOP
        ENDIF
        IF(.NOT. ALL(PTR_CPU(:,2,:) == 2))THEN
            ERROR STOP
        ENDIF
        IF(.NOT. ALL(PTR_CPU(:,3,:) == 3))THEN
            ERROR STOP
        ENDIF

        CALL BUFFER%FINAL()

        DEALLOCATE(FIELDS)
        DEALLOCATE(FIELD_PTRS_GPU)
END PROGRAM INIT_OWNER_FIELD_BUFFER_GPU
