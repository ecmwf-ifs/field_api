! (C) Copyright 2022- ECMWF.
! (C) Copyright 2022- Meteo-France.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.

PROGRAM INIT_WRAPPER_GPU
        ! TEST IF DATA ARE CORRECTLY TRANSFERED ON GPU
        USE FIELD_MODULE
        USE FIELD_FACTORY_MODULE
        USE PARKIND1
        USE FIELD_ABORT_MODULE
        IMPLICIT NONE
        CLASS(FIELD_2RB), POINTER :: W => NULL()
        REAL(KIND=JPRB), ALLOCATABLE :: D(:,:)
        REAL(KIND=JPRB), POINTER :: D_GPU(:,:)
        LOGICAL :: RES
        INTEGER :: I,J

        ALLOCATE(D(10,10))
        D=7

#ifdef _CUDA
        CALL FIELD_NEW(W, DATA=D, MAP_DEVPTR=.FALSE.)
#else
        CALL FIELD_NEW(W, DATA=D)
#endif
        CALL W%GET_DEVICE_DATA_RDONLY(D_GPU)
#ifdef _CUDA
        !$ACC SERIAL DEVICEPTR (D_GPU) COPY(RES)
#else
        !$ACC SERIAL PRESENT (D_GPU) COPY(RES)
#endif
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
                CALL FIELD_ABORT ("ERROR")
        END IF
        CALL FIELD_DELETE(W)
END PROGRAM INIT_WRAPPER_GPU
