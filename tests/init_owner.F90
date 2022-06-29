PROGRAM INIT
        ! TEST IF OWNER IS REALLY ALLOCATING THE DATA

        USE FIELD_MODULE
        IMPLICIT NONE
        TYPE(FIELD_2D_OWNER) :: O

        CALL O%INIT(SHAPE=[10],NBLOCKS=21, PERSISTENT=.TRUE.)
        O%PTR=42

        IF (SIZE(O%PTR,1) /= 10) THEN
                ERROR STOP
        END IF
        IF (SIZE(O%PTR,2) /= 21) THEN
                ERROR STOP
        END IF

        IF (.NOT. ALL(O%PTR == 42)) THEN
                ERROR STOP
        END IF 
END PROGRAM INIT
