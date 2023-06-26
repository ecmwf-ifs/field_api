PROGRAM GET_VIEW
        USE FIELD_module
        USE OMP_LIB

        TYPE(FIELD2RB_WRAPPER) :: W
        REAL(KIND=JPRB), POINTER :: DATA(:,:)
        INTEGER :: NPROMA = 24
        INTEGER :: NBLOCKS= 100
        INTEGER :: IBLK,JLON
        REAL(KIND=JPRB), POINTER :: VIEW(:) => NULL()

        ALLOCATE(DATA(NPROMA, NBLOCKS))
        CALL W%INIT(DATA)
        !$OMP PARALLEL PRIVATE(VIEW, JLON) 
        !$OMP DO
        DO IBLK=1,NBLOCKS
          VIEW => W%GET_VIEW(IBLK)
          DO JLON = 1, NPROMA
            VIEW(JLON) = 7
          ENDDO
        END DO
        !$OMP END DO
        !$OMP END PARALLEL
        IF (.NOT. ALL(DATA == 7)) THEN
                ERROR STOP
        END IF
END PROGRAM GET_VIEW
