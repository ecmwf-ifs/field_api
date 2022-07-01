PROGRAM INIT_OWNER
        ! TEST IF OWNER IS REALLY ALLOCATING THE DATA

        USE FIELD_MODULE
        IMPLICIT NONE
        TYPE(FIELD_2D_OWNER) :: O

        CALL O%INIT([10,1], [21,11])
        O%PTR=42

        IF (SIZE(O%PTR,1) /= 12) THEN
                ERROR STOP
        END IF
        IF (SIZE(O%PTR,2) /= 11) THEN
                ERROR STOP
        END IF

        IF (.NOT. ALL(O%PTR == 42)) THEN
                ERROR STOP
        END IF 
END PROGRAM INIT_OWNER
