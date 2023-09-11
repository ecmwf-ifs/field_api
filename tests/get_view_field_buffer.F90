! (C) Copyright 2022- ECMWF.
! (C) Copyright 2022- Meteo-France.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.

PROGRAM GET_VIEW_FIELD_BUFFER
        USE FIELD_MODULE
        USE FIELD_BUFFER_MODULE
        USE OMP_LIB
        USE PARKIND1

        TYPE(FIELD_3RB_BUFFER) :: W
        REAL(KIND=JPRB), POINTER :: D(:,:,:)

        REAL(KIND=JPRB), POINTER :: BUFFER_VIEW(:,:) => NULL()
        REAL(KIND=JPRB), POINTER :: FIELD_VIEW(:) => NULL()
        TYPE(FIELD_2RB_PTR), ALLOCATABLE :: FIELDS(:)

        INTEGER :: NPROMA = 24
        INTEGER :: NBLOCKS= 100
        INTEGER :: IBLK,JLON,NFIELDS,IFIELD

        NFIELDS = 3

        ALLOCATE(FIELDS(NFIELDS))
        ALLOCATE(D(NPROMA, NBLOCKS, NFIELDS))

        CALL W%INIT(NFIELDS, FIELDS, DATA=D, CONTIG_FIELDS=.TRUE.)
        IF( SIZE(W%BUFFER%PTR, 3) /= NFIELDS )THEN
            ERROR STOP
        ENDIF

        D = 0
        !$OMP PARALLEL PRIVATE(VIEW, JLON) 
        !$OMP DO
        DO IBLK=1,NBLOCKS
          BUFFER_VIEW => W%GET_VIEW(IBLK)
          DO JLON = 1, NPROMA
            BUFFER_VIEW(JLON,1) = 7
            BUFFER_VIEW(JLON,2) = 7
            BUFFER_VIEW(JLON,3) = 7
          ENDDO
        END DO
        !$OMP END DO
        !$OMP END PARALLEL
        IF (.NOT. ALL(D == 7)) THEN
                ERROR STOP
        END IF

        !$OMP PARALLEL PRIVATE(VIEW, JLON) 
        !$OMP DO
        DO IBLK=1,NBLOCKS
          FIELD_VIEW => FIELDS(1)%PTR%GET_VIEW(IBLK)
          DO JLON = 1, NPROMA
            FIELD_VIEW(JLON) = 1
          ENDDO
          FIELD_VIEW => FIELDS(2)%PTR%GET_VIEW(IBLK)
          DO JLON = 1, NPROMA
            FIELD_VIEW(JLON) = 2
          ENDDO
          FIELD_VIEW => FIELDS(3)%PTR%GET_VIEW(IBLK)
          DO JLON = 1, NPROMA
            FIELD_VIEW(JLON) = 3
          ENDDO
        END DO
        !$OMP END DO
        !$OMP END PARALLEL
        IF (.NOT. ALL(D(:,:,1) == 1)) THEN
                ERROR STOP
        END IF
        IF (.NOT. ALL(D(:,:,2) == 2)) THEN
                ERROR STOP
        END IF
        IF (.NOT. ALL(D(:,:,3) == 3)) THEN
                ERROR STOP
        END IF

        CALL W%FINAL()
        DEALLOCATE(FIELDS)
        DEALLOCATE(D)

END PROGRAM GET_VIEW_FIELD_BUFFER
