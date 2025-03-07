! (C) Copyright 2022- ECMWF.
! (C) Copyright 2022- Meteo-France.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.

PROGRAM TEST_GET_DEVICE_DATA_NON_CONTIGUOUS
        USE FIELD_MODULE
        USE FIELD_FACTORY_MODULE
        USE PARKIND1
        USE FIELD_ABORT_MODULE
        IMPLICIT NONE
        CLASS(FIELD_2RB), POINTER     :: W => NULL()
        REAL(KIND=JPRB),  ALLOCATABLE :: D(:,:,:)
        REAL(KIND=JPRB),  POINTER :: PTR_CPU(:,:)
        REAL(KIND=JPRB),  POINTER :: PTR_GPU(:,:)
        LOGICAL :: OKAY
        INTEGER :: I,J

        ALLOCATE(D(-4:3, 1:5, -4:3))
        D= 11
        CALL FIELD_NEW(W, DATA=D(:,2,:), LBOUNDS=[-4,-4])
        CALL W%GET_HOST_DATA_RDWR(PTR_CPU)
        PTR_CPU=42

        CALL W%GET_DEVICE_DATA_RDWR(PTR_GPU)
        OKAY=.TRUE.
#ifdef OMPGPU
        !$OMP TARGET MAP(TO:PTR_GPU) MAP(TOFROM:OKAY)
#else
        !$ACC SERIAL PRESENT (PTR_GPU) COPY(OKAY)
#endif
        DO I=-4,3
        DO J=-4,3
        IF(PTR_GPU(I,J) /= 42) THEN
                OKAY = .FALSE.
        END IF
        END DO
        END DO
#ifdef OMPGPU
        !$OMP END TARGET
#else
        !$ACC END SERIAL
#endif

        IF (.NOT. OKAY) THEN
                CALL FIELD_ABORT ("PTR_GPU differ from 42")
        END IF
        CALL FIELD_DELETE(W)
END PROGRAM TEST_GET_DEVICE_DATA_NON_CONTIGUOUS

