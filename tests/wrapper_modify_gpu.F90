PROGRAM INIT
        ! TEST IF DATA ARE CORRECTLY TRANSFERED ON GPU
        USE FIELD_MODULE
        IMPLICIT NONE
        TYPE(FIELD_2D_WRAPPER) :: W
        REAL(KIND=JPRB), ALLOCATABLE :: D(:,:)
        REAL(KIND=JPRB), POINTER :: D_GPU(:,:), D_CPU(:,:)
        LOGICAL :: RES
        INTEGER :: I,J

        ALLOCATE(D(10,10))
        D=7

        CALL W%INIT(D)
        CALL W%MOVE_DATA_TO_DEVICE_RDWR(D_GPU)

        !$ACC SERIAL PRESENT(D_GPU) COPY(RES)
        RES=.TRUE.
        DO I=1,10
        DO J=1,10
        D_GPU(J,I)=I*J
        END DO
        END DO
        !$ACC END SERIAL


        ! DATA SHOULD ALL BE 7 BECAUSE THEY HAVEN'T MOVED BACK FROM GPU
        ! YET
        IF ( .NOT. ALL(D == 7))THEN
                ERROR STOP
        END IF

        CALL W%MOVE_DATA_FROM_DEVICE_RDONLY(D_CPU)

        DO I=1,10
        DO J=1,10
        IF (D_CPU(J,I)/=I*J) THEN
                ERROR STOP
        END IF
        END DO
        END DO
END PROGRAM INIT
