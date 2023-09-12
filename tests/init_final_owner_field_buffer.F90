! (C) Copyright 2022- ECMWF.
! (C) Copyright 2022- Meteo-France.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.

PROGRAM INIT_FINAL_OWNER_FIELD_BUFFER
        ! TEST IF OWNER IS REALLY ALLOCATING THE DATA
        ! WHEN PERSITENT IS SET TO TRUE OR IS NOT GIVEN IN ARGUMENT,
        ! THEN THE LAST DIM OF THE FIELD IS THE NUMBER OF OPENMP THREADS

        USE FIELD_MODULE
        USE FIELD_BUFFER_MODULE
        USE PARKIND1
        USE OMP_LIB, ONLY: OMP_GET_MAX_THREADS

        IMPLICIT NONE

        TYPE(FIELD_3RB_BUFFER) :: BUFFER
        TYPE(FIELD_2RB_PTR), ALLOCATABLE :: FIELDS(:)
        INTEGER(KIND=JPIM) :: NFIELDS, IFIELD
        REAL(KIND=JPRB), POINTER :: BUFFER_PTR(:,:,:)
        REAL(KIND=JPRB), POINTER :: FIELD_PTR(:,:)

        NFIELDS = 3
        ALLOCATE(FIELDS(NFIELDS))
        
        CALL BUFFER%INIT(NFIELDS, FIELDS, LBOUNDS=[10,1,1], UBOUNDS=[21,NFIELDS,10])

        BUFFER%BUFFER%PTR = 0._JPRB
        CALL BUFFER%GET_HOST_DATA_RDWR(BUFFER_PTR)

        IF(.NOT. ALL(BUFFER_PTR == 0.))THEN
           ERROR STOP
        ENDIF 

        BUFFER_PTR(:,1,:) = 1._JPRB
        BUFFER_PTR(:,2,:) = 2._JPRB
        BUFFER_PTR(:,3,:) = 3._JPRB

        CALL FIELDS(1)%PTR%GET_HOST_DATA_RDWR(FIELD_PTR)
        IF(.NOT. ALL(FIELD_PTR == 1.))THEN
           ERROR STOP
        ENDIF 
        CALL FIELDS(2)%PTR%GET_HOST_DATA_RDWR(FIELD_PTR)
        IF(.NOT. ALL(FIELD_PTR == 2.))THEN
           ERROR STOP
        ENDIF 
        CALL FIELDS(3)%PTR%GET_HOST_DATA_RDWR(FIELD_PTR)
        IF(.NOT. ALL(FIELD_PTR == 3.))THEN
           ERROR STOP
        ENDIF 

        IF (SIZE(BUFFER%BUFFER%PTR,1) /= 12) THEN
                ERROR STOP
        END IF
        IF (SIZE(BUFFER%BUFFER%PTR,2) /= NFIELDS) THEN
                ERROR STOP
        END IF
        IF (SIZE(BUFFER%BUFFER%PTR,3) /= OMP_GET_MAX_THREADS()) THEN
                ERROR STOP
        END IF

        DO IFIELD=1,NFIELDS
           IF (SIZE(FIELDS(IFIELD)%PTR%PTR,1) /= 12) THEN
                   ERROR STOP
           END IF
           IF (SIZE(FIELDS(IFIELD)%PTR%PTR,2) /= OMP_GET_MAX_THREADS()) THEN
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

        DEALLOCATE(FIELDS)
END PROGRAM INIT_FINAL_OWNER_FIELD_BUFFER
