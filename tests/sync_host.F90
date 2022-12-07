PROGRAM SYNC_HOST
        USE FIELD_MODULE
        IMPLICIT NONE

        TYPE(FIELD_2D_WRAPPER) :: W
        REAL(KIND=JPRB), ALLOCATABLE :: D(:,:)
        REAL(KIND=JPRB), POINTER :: D_GPU(:,:)
        INTEGER :: I, J

        ALLOCATE(D(10,10))
        D=3
        CALL W%INIT(D)
        CALL W%GET_DEVICE_DATA_RDWR(D_GPU)
!$ACC KERNELS PRESENT(D_GPU)        
        DO I=1,10
        DO J=1,10
        D_GPU(I,J) = 7
        ENDDO
        ENDDO
!$ACC END KERNELS

        CALL W%SYNC_HOST_RDONLY()
        DO I=1,10
        DO J=1,10
        IF (D(I,J) /= 7) THEN
                STOP
        ENDIF
        ENDDO
        ENDDO
END PROGRAM
