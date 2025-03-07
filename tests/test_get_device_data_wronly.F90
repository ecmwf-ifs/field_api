! (C) Copyright 2022- ECMWF.
! (C) Copyright 2022- Meteo-France.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.

PROGRAM TEST_GET_DEVICE_DATA_WRONLY
        ! TEST THAT DATA IS NOT COPIED TO GPU FOR WRONLY ACCESS

        USE FIELD_MODULE
        USE FIELD_FACTORY_MODULE
        USE PARKIND1
        USE FIELD_ABORT_MODULE
        IMPLICIT NONE
        CLASS(FIELD_2RB), POINTER :: O => NULL()
        REAL(KIND=JPRB), POINTER :: PTR_CPU(:,:)
        REAL(KIND=JPRB), POINTER :: PTR_GPU(:,:)
        LOGICAL :: OKAY
        INTEGER :: I,J

        CALL FIELD_NEW(O, LBOUNDS=[10,1], UBOUNDS=[21,11], PERSISTENT=.TRUE.)
        CALL O%GET_HOST_DATA_RDWR(PTR_CPU)
        PTR_CPU=42

        CALL O%GET_DEVICE_DATA_WRONLY(PTR_GPU)
        OKAY=.TRUE.
#ifdef OMPGPU
        !$OMP TARGET MAP(TO:PTR_GPU) MAP(TOFROM:OKAY)
#else
        !$ACC SERIAL PRESENT (PTR_GPU) COPY(OKAY)
#endif
        DO I=10,21
        DO J=1,11
        IF(PTR_GPU(I,J) == 42) THEN
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
                CALL FIELD_ABORT ("ERROR")
        END IF
        CALL FIELD_DELETE(O)
END PROGRAM TEST_GET_DEVICE_DATA_WRONLY
