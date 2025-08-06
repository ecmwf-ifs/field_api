! (C) Copyright 2022- ECMWF.
! (C) Copyright 2022- Meteo-France.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.

PROGRAM INIT_OWNER_DELAYED_INIT_VALUE
        !TEST THAT ALL DATA ARE INITIALIZE TO THE CHOOSEN VALUE
        USE FIELD_MODULE
        USE FIELD_FACTORY_MODULE
        USE PARKIND1
        USE OMP_LIB, ONLY: OMP_GET_MAX_THREADS
        USE FIELD_ABORT_MODULE
        IMPLICIT NONE
        CLASS(FIELD_2IM), POINTER :: O => NULL()
        INTEGER(KIND=JPIM), POINTER :: PTR(:,:)
        INTEGER :: I, J, MAX_THREADS
        LOGICAL :: OKAY

        CALL FIELD_NEW(O, LBOUNDS=[1,1], UBOUNDS=[10,10], INIT_VALUE=7_JPIM, DELAYED=.TRUE.)
        CALL O%GET_DEVICE_DATA_RDONLY(PTR)
        MAX_THREADS=OMP_GET_MAX_THREADS()

        OKAY=.TRUE.
#ifdef OMPGPU
        !$OMP TARGET TEAMS DISTRIBUTE PARALLEL DO MAP(TO:PTR, MAX_THREADS) MAP(TOFROM:OKAY)
#else
        !$ACC KERNELS PRESENT(PTR) COPY(OKAY)
#endif
        DO I=1,10
          DO J=1,MAX_THREADS
            IF(PTR(I,J) /= 7) THEN
              OKAY = .FALSE.
            ENDIF
          ENDDO
        ENDDO
#ifdef OMPGPU
        !$OMP END TARGET TEAMS DISTRIBUTE PARALLEL DO
#else
        !$ACC END KERNELS
#endif

        IF(.NOT. OKAY) THEN
          CALL FIELD_ABORT("ERROR")
        ENDIF

        PTR=>NULL()
        CALL O%GET_HOST_DATA_RDONLY(PTR)
        DO I=1,10
          DO J=1,MAX_THREADS
            IF(PTR(I,J) /= 7) THEN
              OKAY = .FALSE.
            ENDIF
          ENDDO
        ENDDO

        IF(.NOT. OKAY) THEN
          CALL FIELD_ABORT("ERROR")
        ENDIF

        CALL FIELD_DELETE(O)
END PROGRAM INIT_OWNER_DELAYED_INIT_VALUE
