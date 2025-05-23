! (C) Copyright 2022- ECMWF.
! (C) Copyright 2022- Meteo-France.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.

PROGRAM SYNC_HOST
        USE FIELD_MODULE
        USE FIELD_FACTORY_MODULE
        USE PARKIND1
        USE FIELD_ABORT_MODULE
        IMPLICIT NONE

        CLASS(FIELD_2RB), POINTER :: W => NULL()
        REAL(KIND=JPRB), ALLOCATABLE :: D(:,:)
        REAL(KIND=JPRB), POINTER :: D_GPU(:,:)
        INTEGER :: I, J

        ALLOCATE(D(10,10))
        D=3
        CALL FIELD_NEW(W, DATA=D)
        CALL W%GET_DEVICE_DATA_RDWR(D_GPU)
#ifdef OMPGPU
!$OMP TARGET TEAMS DISTRIBUTE PARALLEL DO MAP(TO:D_GPU)
#else
!$ACC KERNELS PRESENT(D_GPU)
#endif
        DO I=1,10
        DO J=1,10
        D_GPU(I,J) = 7
        ENDDO
        ENDDO
#ifdef OMPGPU
!$OMP END TARGET TEAMS DISTRIBUTE PARALLEL DO
#else
!$ACC END KERNELS
#endif

        CALL W%SYNC_HOST_RDONLY()
        DO I=1,10
        DO J=1,10
        IF (D(I,J) /= 7) THEN
                CALL FIELD_ABORT ("ERROR")
        ENDIF
        ENDDO
        ENDDO
        CALL FIELD_DELETE(W)
END PROGRAM
