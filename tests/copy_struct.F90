! (C) Copyright 2022- ECMWF.
! (C) Copyright 2022- Meteo-France.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.

PROGRAM COPY_STRUCT
        USE FIELD_MODULE
        USE FIELD_FACTORY_MODULE
        USE PARKIND1
        IMPLICIT NONE

        CLASS(FIELD_2RB), POINTER :: W => NULL()
        REAL(KIND=JPRB), ALLOCATABLE :: D(:,:)
        TYPE(FIELD_3RB_VIEW) :: STRUCT
        INTEGER :: I, J

        ALLOCATE(D(10,10))
        D=3
        CALL FIELD_NEW(W, DATA=D)
        CALL W%GET_DEVICE_DATA_RDWR(STRUCT%P)
#ifdef OMPGPU
        !$OMP TARGET TEAMS DISTRIBUTE PARALLEL DO MAP(TO:STRUCT,STRUCT%P)
#else
        !$ACC KERNELS COPYIN(W) PRESENT(W%DEVPTR)
#endif
        DO I=1,10
        DO J=1,10
        STRUCT%P(I,J) = 7
        ENDDO
        ENDDO
#ifdef OMPGPU
        !$OMP END TARGET TEAMS DISTRIBUTE PARALLEL DO
#else
        !$ACC END KERNELS
#endif
        CALL FIELD_DELETE(W)
END PROGRAM
