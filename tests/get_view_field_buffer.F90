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
        USE FIELD_FACTORY_MODULE
        USE FIELD_INIT_DEBUG_VALUE_MODULE, ONLY : INIT_DEBUG_VALUE_JPRB
        USE OMP_LIB
        USE PARKIND1

        CLASS(FIELD_3RB), POINTER :: W => NULL()
        CLASS(FIELD_3RB), POINTER :: O => NULL()
        REAL(KIND=JPRB), POINTER :: D(:,:,:)

        REAL(KIND=JPRB), POINTER :: BUFFER_VIEW(:,:) => NULL()
        REAL(KIND=JPRB), POINTER :: FIELD_VIEW(:) => NULL()
        TYPE(FIELD_2RB_PTR), ALLOCATABLE :: FIELDS_W(:)
        TYPE(FIELD_2RB_PTR), ALLOCATABLE :: FIELDS_O(:)

        INTEGER :: NPROMA = 24
        INTEGER :: NBLOCKS= 100
        INTEGER :: IBLK,JLON,NFIELDS,IFIELD

        INTEGER (KIND=JPIM), PARAMETER :: NDEVFRESH = INT(B'00000001', KIND=JPIM)

        NFIELDS = 3

        ALLOCATE(D(NPROMA, NBLOCKS, NFIELDS))

        CALL FIELD_NEW(W, NFIELDS, FIELDS_W, DATA=D, CONTIG_FIELDS=.TRUE.)
        CALL FIELD_NEW(O, NFIELDS, FIELDS_O, UBOUNDS=[NPROMA,NFIELDS,NBLOCKS])

        ! Check buffer layout
        IF( SIZE(W%PTR, 3) /= NFIELDS )THEN
            ERROR STOP
        ENDIF
        IF( SIZE(O%PTR, 2) /= NFIELDS )THEN
            ERROR STOP
        ENDIF

        ! Confirm the owned buffer was initialised
        IF (.NOT. ALL(O%PTR == INIT_DEBUG_VALUE_JPRB)) THEN
                ERROR STOP
        END IF

        D = 0
        !$OMP PARALLEL PRIVATE(BUFFER_VIEW, JLON) 
        !$OMP DO
        DO IBLK=1,NBLOCKS
          BUFFER_VIEW => W%GET_VIEW(IBLK)
          DO JLON = 1, NPROMA
            BUFFER_VIEW(JLON,1) = 7
            BUFFER_VIEW(JLON,2) = 7
            BUFFER_VIEW(JLON,3) = 7
          ENDDO
          BUFFER_VIEW => O%GET_VIEW(IBLK)
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
        IF (.NOT. ALL(O%PTR == 7)) THEN
                ERROR STOP
        END IF

        ! Check buffer is marked dirty on device
        IF (IAND (W%ISTATUS, NDEVFRESH) /= 0) THEN
          ERROR STOP
        ENDIF
        IF (IAND (O%ISTATUS, NDEVFRESH) /= 0) THEN
          ERROR STOP
        ENDIF

        ! Check children are marked dirty on device
        DO IFIELD=1,NFIELDS
          IF (IAND (FIELDS_W(IFIELD)%PTR%ISTATUS, NDEVFRESH) /= 0) THEN
            ERROR STOP
          ENDIF
          IF (IAND (FIELDS_O(IFIELD)%PTR%ISTATUS, NDEVFRESH) /= 0) THEN
            ERROR STOP
          ENDIF
        ENDDO

        !$OMP PARALLEL PRIVATE(FIELD_VIEW, JLON) 
        !$OMP DO
        DO IBLK=1,NBLOCKS
          FIELD_VIEW => FIELDS_W(1)%PTR%GET_VIEW(IBLK)
          DO JLON = 1, NPROMA
            FIELD_VIEW(JLON) = 1
          ENDDO
          FIELD_VIEW => FIELDS_W(2)%PTR%GET_VIEW(IBLK)
          DO JLON = 1, NPROMA
            FIELD_VIEW(JLON) = 2
          ENDDO
          FIELD_VIEW => FIELDS_W(3)%PTR%GET_VIEW(IBLK)
          DO JLON = 1, NPROMA
            FIELD_VIEW(JLON) = 3
          ENDDO

          FIELD_VIEW => FIELDS_O(1)%PTR%GET_VIEW(IBLK)
          DO JLON = 1, NPROMA
            FIELD_VIEW(JLON) = 1
          ENDDO
          FIELD_VIEW => FIELDS_O(2)%PTR%GET_VIEW(IBLK)
          DO JLON = 1, NPROMA
            FIELD_VIEW(JLON) = 2
          ENDDO
          FIELD_VIEW => FIELDS_O(3)%PTR%GET_VIEW(IBLK)
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
        IF (.NOT. ALL(O%PTR(:,1,:) == 1)) THEN
                ERROR STOP
        END IF
        IF (.NOT. ALL(O%PTR(:,2,:) == 2)) THEN
                ERROR STOP
        END IF
        IF (.NOT. ALL(O%PTR(:,3,:) == 3)) THEN
                ERROR STOP
        END IF

        CALL FIELD_DELETE(W, FIELDS_W)
        CALL FIELD_DELETE(O, FIELDS_O)

        IF(ALLOCATED(FIELDS_W)) ERROR STOP
        IF(ALLOCATED(FIELDS_O)) ERROR STOP
        DEALLOCATE(D)

END PROGRAM GET_VIEW_FIELD_BUFFER
