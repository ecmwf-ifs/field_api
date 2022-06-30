PROGRAM INIT
        ! TEST IF OWNER IS DESTROYED WHEN CALLING FINAL

        USE FIELD_MODULE
        IMPLICIT NONE
        TYPE(FIELD_2D_OWNER) :: O

        CALL O%INIT([10,1],[21,11])
        O%PTR=42
        CALL O%FINAL()

        IF (ASSOCIATED(O%PTR)) THEN
                ERROR STOP
        END IF
        IF (ASSOCIATED(O%DEVPTR)) THEN
                ERROR STOP
        END IF
END PROGRAM INIT