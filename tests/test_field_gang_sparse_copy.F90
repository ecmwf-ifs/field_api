! (C) Copyright 2022- ECMWF.
! (C) Copyright 2022- Meteo-France.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.

PROGRAM TEST_FIELD_GANG_SPARSE_COPY
        ! TEST THE SPARSE COPY MECHANISM FOR THE FIELD GANG

        USE FIELD_MODULE
        USE FIELD_FACTORY_MODULE
        USE FIELD_CONSTANTS_MODULE
        USE PARKIND1

        IMPLICIT NONE

        CLASS(FIELD_3RB), POINTER :: BUFFER => NULL()
        TYPE(FIELD_2RB_PTR), ALLOCATABLE :: FIELDS(:)

        REAL(KIND=JPRB), POINTER :: BUFFER_CPU(:,:,:)
        REAL(KIND=JPRB), POINTER :: BUFFER_GPU(:,:,:)
        REAL(KIND=JPRB), POINTER :: FIELD_PTR(:,:)

        LOGICAL :: RES
        INTEGER(KIND=JPIM) :: NFIELDS, IFIELD, I, J

        NFIELDS = 3
        CALL FIELD_NEW(BUFFER, FIELDS, LBOUNDS=[10,1,1], UBOUNDS=[21,NFIELDS,10], PERSISTENT=.TRUE., INIT_VALUE=0._JPRB)

        CALL BUFFER%GET_DEVICE_DATA_RDWR(BUFFER_GPU)
        !$ACC SERIAL PRESENT (BUFFER_GPU)
        DO J=1,10
          DO IFIELD=1,NFIELDS
            DO I=10,21
              BUFFER_GPU(I,IFIELD,J) = 42._JPRB
            END DO
          END DO
        END DO
        !$ACC END SERIAL

        CALL BUFFER%GET_HOST_DATA_RDWR(BUFFER_CPU)
        IF(.NOT. ALL(BUFFER_CPU == 42)) ERROR STOP

        CALL FIELDS(1)%PTR%GET_HOST_DATA_RDWR(FIELD_PTR)
        IF(.NOT. ALL(FIELD_PTR == 42)) ERROR STOP
        CALL FIELDS(2)%PTR%GET_HOST_DATA_RDWR(FIELD_PTR)
        IF(.NOT. ALL(FIELD_PTR == 42)) ERROR STOP
        CALL FIELDS(3)%PTR%GET_HOST_DATA_RDWR(FIELD_PTR)
        IF(.NOT. ALL(FIELD_PTR == 42)) ERROR STOP

        CALL FIELDS(2)%PTR%GET_DEVICE_DATA_RDWR(FIELD_PTR)

        !$ACC SERIAL PRESENT (FIELD_PTR)
        DO J=1,10
          DO I=10,21
             FIELD_PTR(I,J) = 1
          END DO
        END DO
        !$ACC END SERIAL

        ! Check that children and parent have different statuses
        IF(IAND(FIELDS(1)%PTR%ISTATUS, NDEVFRESH) /= 0) ERROR STOP
        IF(IAND(FIELDS(2)%PTR%ISTATUS, NHSTFRESH) /= 0) ERROR STOP
        IF(IAND(FIELDS(3)%PTR%ISTATUS, NDEVFRESH) /= 0) ERROR STOP
        IF(IAND(BUFFER%ISTATUS, NDEVFRESH) /= 0) ERROR STOP

        CALL BUFFER%GET_DEVICE_DATA_RDWR(BUFFER_GPU)
        RES=.TRUE.
        !$ACC SERIAL PRESENT (BUFFER_GPU) COPY(RES)
        DO J=1,10
          DO I=10,21
            IF(BUFFER_GPU(I,1,J) /= 42) THEN
                    RES = .FALSE.
            END IF
            IF(BUFFER_GPU(I,2,J) /= 1) THEN
                    RES = .FALSE.
            END IF
            IF(BUFFER_GPU(I,3,J) /= 42) THEN
                    RES = .FALSE.
            END IF
          END DO
        END DO
        !$ACC END SERIAL
        IF(.NOT. RES) ERROR STOP

        ! Check that children and parent statuses have synchronised
        IF(IAND(BUFFER%ISTATUS, NHSTFRESH) /= 0) ERROR STOP
        DO IFIELD=1,NFIELDS
          IF(IAND(FIELDS(IFIELD)%PTR%ISTATUS, NHSTFRESH) /= 0) ERROR STOP
        ENDDO

        CALL FIELD_DELETE(BUFFER)
        DEALLOCATE(FIELDS)
END PROGRAM TEST_FIELD_GANG_SPARSE_COPY
