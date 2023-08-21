!COPYRIGHT Météo-France 2023
PROGRAM GET_VIEW
        USE FIELD_MODULE
        use FIELD_FACTORY_MODULE
        USE OMP_LIB
        USE PARKIND1

        CLASS(FIELD_2RB), POINTER :: W => NULL()
        REAL(KIND=JPRB), POINTER :: D(:,:)
        INTEGER :: NPROMA = 24
        INTEGER :: NBLOCKS= 100
        INTEGER :: IBLK,JLON
        REAL(KIND=JPRB), POINTER :: VIEW(:) => NULL()

        ALLOCATE(D(NPROMA, NBLOCKS))
        CALL FIELD_NEW(W, DATA=D)
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
        IF (.NOT. ALL(D == 7)) THEN
                ERROR STOP
        END IF
        CALL FIELD_DELETE(W)
END PROGRAM GET_VIEW
