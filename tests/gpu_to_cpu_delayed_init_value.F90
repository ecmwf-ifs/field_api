! (C) Copyright 2022- ECMWF.
! (C) Copyright 2022- Meteo-France.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.

PROGRAM GPU_TO_CPU_DELAYED_INIT_VALUE
        !TEST THAT DATA OF A FIELD OWNER IS NOT COPIED TO GPU WHEN IT HAS
        !BEEN INITIALISED WITH AN INIT VALUE AFTER A DELAYED ALLOCATION, BUT
        !THAT IT IS TRANSFERED IF WE WANT TO READ IT FROM CPU

        USE FIELD_MODULE
        USE FIELD_FACTORY_MODULE
        USE PARKIND1
        USE FIELD_ABORT_MODULE
        IMPLICIT NONE
        CLASS(FIELD_2IM), POINTER :: O => NULL()
        INTEGER(KIND=JPIM), POINTER :: PTR_HOST(:,:) => NULL()
        INTEGER(KIND=JPIM), POINTER :: PTR_DEV(:,:) => NULL()
        INTEGER ::I,J
        LOGICAL :: OKAY

        CALL FIELD_NEW(O, LBOUNDS=[1,1],UBOUNDS=[5,5], PERSISTENT=.TRUE., DELAYED=.TRUE., INIT_VALUE=3)

        CALL O%GET_DEVICE_DATA_RDONLY(PTR_DEV)
        WRITE(*,*)"O%STATS%TRANSFER_CPU_TO_GPU",O%STATS%TRANSFER_CPU_TO_GPU
        WRITE(*,*)"O%STATS%TRANSFER_GPU_TO_CPU",O%STATS%TRANSFER_GPU_TO_CPU
        IF(O%STATS%TRANSFER_GPU_TO_CPU /= 0 .OR. O%STATS%TRANSFER_CPU_TO_GPU /= 0)THEN
          CALL FIELD_ABORT ("ERROR NO TRANSFER SHOULD HAVE HAPPENED")
        ENDIF
        OKAY=.TRUE.
#ifdef OMPGPU
        !$OMP TARGET TEAMS DISTRIBUTE PARALLEL DO MAP(TOFROM: OKAY)
#else
        !$ACC PARALLEL PRESENT(PTR_DEV) COPY(OKAY)
#endif
        DO I=1,5
          DO J=1,5
          IF (PTR_DEV(I,J) /= 3)THEN
            OKAY=.FALSE.
          ENDIF
          ENDDO
        ENDDO
#ifdef OMPGPU
        !$OMP END TARGET TEAMS DISTRIBUTE PARALLEL DO
#else
        !$ACC END PARALLEL
#endif
        IF(OKAY .EQV. .FALSE.)THEN
          CALL FIELD_ABORT ("ERROR, NOT ALL VALUES WERE CORRECT")
        ENDIF

        CALL O%GET_HOST_DATA_RDWR(PTR_DEV)
        WRITE(*,*)"O%STATS%TRANSFER_CPU_TO_GPU",O%STATS%TRANSFER_CPU_TO_GPU
        WRITE(*,*)"O%STATS%TRANSFER_GPU_TO_CPU",O%STATS%TRANSFER_GPU_TO_CPU
        IF(O%STATS%TRANSFER_GPU_TO_CPU /= 1)THEN
          CALL FIELD_ABORT ("ONE TRANSFER IN EACH DIRECTION SHOULD HAVE HEPPENED")
        ENDIF

        DO I=1,5
          DO J=1,5
            IF (PTR_DEV(I,J) /= 3)THEN
              CALL FIELD_ABORT ("ERROR")
            ENDIF
          ENDDO
        ENDDO

        CALL FIELD_DELETE(O)
END PROGRAM GPU_TO_CPU_DELAYED_INIT_VALUE
