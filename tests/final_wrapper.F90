PROGRAM INIT
        ! TEST IF DESTROYING THE WRAPPER DOESN'T DESTROY THE WRAPPED DATA

        USE FIELD_MODULE
        IMPLICIT NONE
        TYPE(FIELD_2D_WRAPPER) :: W
        REAL(KIND=JPRB), ALLOCATABLE :: D(:,:)

        ALLOCATE(D(10,10))
        D=7

        CALL W%INIT(D)
        CALL W%FINAL()

        IF (ASSOCIATED(W%PTR)) THEN
                ERROR STOP
        END IF
        IF (ASSOCIATED(W%DEVPTR)) THEN
                ERROR STOP
        END IF

        IF (.NOT. ALLOCATED(D)) THEN
                ERROR STOP
        END IF

        IF (.NOT. ALL(D == 7)) THEN
                ERROR STOP
        END IF 

END PROGRAM INIT
