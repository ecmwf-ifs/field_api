PROGRAM INIT_OWNER_INIT_DEBUG_VALUE
        !TEST THAT ALL DATA ARE INITIALIZE TO THE DEBUG VALUE CHOOSED BY THE
        !USER
        USE FIELD_MODULE
        USE FIELD_FACTORY_MODULE
        USE FIELD_INIT_DEBUG_VALUE_MODULE
        USE PARKIND1
        USE OMP_LIB, ONLY: OMP_GET_MAX_THREADS
        IMPLICIT NONE
        CLASS(FIELD_2IM), POINTER :: O => NULL()
        INTEGER(KIND=JPIM), POINTER :: PTR(:,:)

        USE_INIT_DEBUG_VALUE = .TRUE.
        INIT_DEBUG_VALUE_JPIM=-123456789

        CALL FIELD_NEW(O, LBOUNDS=[1,1], UBOUNDS=[10,10])
        CALL O%GET_HOST_DATA_RDWR(PTR)

        WRITE(*,*)O%PTR(1,1)
        IF (.NOT. ALL(O%PTR == -123456789)) THEN
                ERROR STOP
        END IF

        CALL FIELD_DELETE(O)
END PROGRAM INIT_OWNER_INIT_DEBUG_VALUE
