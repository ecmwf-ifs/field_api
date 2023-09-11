! (C) Copyright 2022- ECMWF.
! (C) Copyright 2022- Meteo-France.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.

PROGRAM INIT_FINAL_WRAPPER_FIELD_BUFFER
        ! TEST IF WRAPPER IS REALLY WRAPPING THE DATA

        USE FIELD_MODULE
        USE FIELD_BUFFER_MODULE
        USE PARKIND1
        USE OMP_LIB, ONLY: OMP_GET_MAX_THREADS

        IMPLICIT NONE

        TYPE(FIELD_3RB_BUFFER) :: BUFFER
        TYPE(FIELD_2RB_PTR), ALLOCATABLE :: FIELDS(:)
        INTEGER(KIND=JPIM) :: NFIELDS, IFIELD
        REAL(KIND=JPRB), POINTER :: PTR(:,:,:)
        REAL(KIND=JPRB), ALLOCATABLE :: D(:,:,:)
        TYPE(FIELD_3RB_VIEW), ALLOCATABLE :: FIELD_PTRS(:)

        NFIELDS = 3
        ALLOCATE(FIELDS(NFIELDS))
        ALLOCATE(FIELD_PTRS(NFIELDS))
        ALLOCATE(D(10:21,NFIELDS,2:6))
        
        CALL BUFFER%INIT(NFIELDS, FIELDS, DATA=D)

        D = 0._JPRB
        CALL BUFFER%GET_HOST_DATA_RDWR(PTR, FIELDS=FIELD_PTRS)

        IF(.NOT. ALL(PTR == 0.))THEN
           ERROR STOP
        ENDIF 

        PTR(:,1,:) = 1._JPRB
        PTR(:,2,:) = 2._JPRB
        PTR(:,3,:) = 3._JPRB

        IF(.NOT. ALL(FIELD_PTRS(1)%P == 1.))THEN
           ERROR STOP
        ENDIF 
        IF(.NOT. ALL(FIELD_PTRS(2)%P == 2.))THEN
           ERROR STOP
        ENDIF 
        IF(.NOT. ALL(FIELD_PTRS(3)%P == 3.))THEN
           ERROR STOP
        ENDIF 

        IF (SIZE(BUFFER%BUFFER%PTR,1) /= 12) THEN
                ERROR STOP
        END IF
        IF (SIZE(BUFFER%BUFFER%PTR,2) /= NFIELDS) THEN
                ERROR STOP
        END IF
        IF (SIZE(BUFFER%BUFFER%PTR,3) /= 5) THEN
                ERROR STOP
        END IF

        DO IFIELD=1,NFIELDS
           IF (SIZE(FIELDS(IFIELD)%PTR%PTR,1) /= 12) THEN
                   ERROR STOP
           END IF
           IF (SIZE(FIELDS(IFIELD)%PTR%PTR,2) /= 5) THEN
                   ERROR STOP
           END IF
        ENDDO

        IF(.NOT. ASSOCIATED(BUFFER%BUFFER))THEN
            ERROR STOP
        ENDIF
        IF(.NOT. ASSOCIATED(BUFFER%BUFFER%PTR))THEN
            ERROR STOP
        ENDIF
        DO IFIELD=1,NFIELDS
            IF(.NOT. ASSOCIATED(FIELDS(IFIELD)%PTR))THEN
                ERROR STOP
            ENDIF
            IF(.NOT. ASSOCIATED(FIELDS(IFIELD)%PTR%PTR))THEN
                ERROR STOP
            ENDIF
        ENDDO
        CALL BUFFER%FINAL()
        IF(ASSOCIATED(BUFFER%BUFFER))THEN
            ERROR STOP
        ENDIF
        DO IFIELD=1,NFIELDS
            IF(ASSOCIATED(FIELDS(IFIELD)%PTR))THEN
                ERROR STOP
            ENDIF
        ENDDO

        DEALLOCATE(D)
        DEALLOCATE(FIELDS)
        DEALLOCATE(FIELD_PTRS)
END PROGRAM INIT_FINAL_WRAPPER_FIELD_BUFFER
