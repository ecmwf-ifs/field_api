! (C) Copyright 2022- ECMWF.
! (C) Copyright 2022- Meteo-France.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.

PROGRAM INIT_OWNER_DELAYED_GPU
        !TEST IF ACCESSING THE HOST PTR IS WORKING AFTER HAVING ACCESSED FIRST THE DEVPTR

        USE FIELD_MODULE
        USE FIELD_FACTORY_MODULE
        USE PARKIND1
        USE FIELD_ABORT_MODULE
        USE OML_MOD, ONLY: OML_MAX_THREADS
        IMPLICIT NONE
        CLASS(FIELD_2RB), POINTER :: O => NULL()
        REAL(KIND=JPRB), POINTER :: PTR(:,:)
        LOGICAL :: OKAY
        INTEGER :: I,J, MAX_THREADS

        CALL FIELD_NEW(O, LBOUNDS=[10,1], UBOUNDS=[21,11], DELAYED=.TRUE.)
        CALL O%GET_HOST_DATA_RDWR(PTR)
        PTR=42

        PTR=>NULL()
        CALL O%GET_DEVICE_DATA_RDWR(PTR)

        OKAY=.TRUE.
        MAX_THREADS=OML_MAX_THREADS()
#ifdef OMPGPU
        !$OMP TARGET TEAMS DISTRIBUTE PARALLEL DO MAP(TO:PTR, MAX_THREADS) MAP(TOFROM:OKAY)
#else
        !$ACC KERNELS PRESENT(PTR) COPY(OKAY) COPYIN(MAX_THREADS)
#endif
        DO I=10,21
          DO J=1,MAX_THREADS
            IF(PTR(I,J) /= 42)THEN
              OKAY=.FALSE.
            ENDIF
          ENDDO
        ENDDO
#ifdef OMPGPU
        !$OMP END TARGET TEAMS DISTRIBUTE PARALLEL DO
#else
        !$ACC END KERNELS
#endif

        IF(OKAY .EQV. .FALSE.)THEN
          CALL FIELD_ABORT ("ERROR")
        ENDIF
        CALL FIELD_DELETE(O)
END PROGRAM INIT_OWNER_DELAYED_GPU
