! (C) Copyright 2022- ECMWF.
! (C) Copyright 2022- Meteo-France.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.

PROGRAM CPU_TO_GPU
        !TEST INITIALISING DATA ON CPU THEN MOVING THEM ON GPU

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

        CALL FIELD_NEW(O, LBOUNDS=[1,1], UBOUNDS=[11,11], PERSISTENT=.TRUE.)
        CALL O%GET_HOST_DATA_RDWR(PTR_HOST)
        PTR_HOST=7

        IF (.NOT. ALL(PTR_HOST == 7)) THEN
                CALL FIELD_ABORT ("ERROR")
        END IF 

        CALL O%GET_DEVICE_DATA_RDONLY(PTR_DEV)

        OKAY=.TRUE.
#ifdef OMPGPU
        !$OMP TARGET MAP(TO:PTR_DEV) MAP(TOFROM:OKAY)
#else
        !$ACC PARALLEL PRESENT(PTR_DEV) COPY(OKAY)
#endif
        DO I=1,11
        DO J=1,11
          IF(PTR_DEV(I,J)/=7)THEN
                  OKAY=.FALSE.
          ENDIF
        ENDDO
        ENDDO
#ifdef OMPGPU
        !$OMP END TARGET
#else
        !$ACC END PARALLEL
#endif
        IF(OKAY .EQV. .FALSE.)THEN
                CALL FIELD_ABORT ("ERROR")
        ENDIF
        CALL FIELD_DELETE(O)
END PROGRAM CPU_TO_GPU
