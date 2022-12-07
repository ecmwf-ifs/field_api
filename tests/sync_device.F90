PROGRAM SYNC_HOST
#ifdef _OPENACC
        USE FIELD_MODULE
        IMPLICIT NONE

        TYPE(FIELD_2D_WRAPPER) :: W
        REAL(KIND=JPRB), ALLOCATABLE :: D(:,:)
        INTEGER :: I, J

        ALLOCATE(D(10,10))
        D=3
        CALL W%INIT(D)
        CALL W%SYNC_DEVICE_RDWR()
        !$ACC KERNELS COPYIN(W) PRESENT(W%DEVPTR)
        DO I=1,10
        DO J=1,10
        W%DEVPTR(I,J) = 7
        ENDDO
        ENDDO
        !$ACC END KERNELS
#endif //_OPENACC
END PROGRAM
