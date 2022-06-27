PROGRAM INIT
        ! TEST IF DATA ARE CORRECTLY TRANSFERED ON GPU

        USE FIELD_MODULE
        IMPLICIT NONE
        TYPE(FIELD_2D_OWNER) :: O
        REAL(KIND=JPRB), POINTER :: D_GPU(:,:)
        LOGICAL :: RES
        INTEGER :: I,J

        CALL O%INIT(SHAPE=[10],NBLOCKS=21, PERSISTENT=.TRUE.)
        O%PTR=42

        CALL O%MOVE_DATA_TO_DEVICE_RDONLY(D_GPU)
        !$ACC SERIAL PRESENT(D_GPU) COPY(RES)
        RES=.TRUE.
        DO I=1,10
        DO J=1,10
        IF(D_GPU(I,J) /= 42) THEN
                RES = .FALSE.
        END IF
        END DO
        END DO
        !$ACC END SERIAL

        IF (RES) THEN
                STOP
        ELSE
                ERROR STOP
        END IF
END PROGRAM INIT
