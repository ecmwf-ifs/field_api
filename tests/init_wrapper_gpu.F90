PROGRAM INIT_WRAPPER_GPU
        ! TEST IF DATA ARE CORRECTLY TRANSFERED ON GPU
        USE FIELD_MODULE
        IMPLICIT NONE
        TYPE(FIELD2RB_WRAPPER) :: W
        REAL(KIND=JPRB), ALLOCATABLE :: D(:,:)
        REAL(KIND=JPRB), POINTER :: D_GPU(:,:)
        LOGICAL :: RES
        INTEGER :: I,J

        ALLOCATE(D(10,10))
        D=7

        CALL W%INIT(D)
        CALL W%GET_DEVICE_DATA_RDONLY(D_GPU)
        !$ACC SERIAL PRESENT(D_GPU) COPY(RES)
        RES=.TRUE.
        DO I=1,10
        DO J=1,10
        IF(D_GPU(I,J) /= 7) THEN
                RES = .FALSE.
        END IF
        END DO
        END DO
        !$ACC END SERIAL

        IF (.NOT. RES) THEN
                ERROR STOP
        END IF
END PROGRAM INIT_WRAPPER_GPU
