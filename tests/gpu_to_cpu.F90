! (C) Copyright 2022- ECMWF.
! (C) Copyright 2022- Meteo-France.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.

PROGRAM GPU_TO_CPU
        !TEST INITIALISING DATA ON GPU THEN MOVING THEM ON CPU

        USE FIELD_MODULE
        USE FIELD_FACTORY_MODULE
        USE PARKIND1
        USE FIELD_ABORT_MODULE
        IMPLICIT NONE
        CLASS(FIELD_2IM), POINTER :: O => NULL()
        INTEGER(KIND=JPIM), POINTER :: PTR_HOST(:,:) => NULL()
        INTEGER(KIND=JPIM), POINTER :: PTR_DEV(:,:) => NULL()
        INTEGER ::I,J

        CALL FIELD_NEW(O, LBOUNDS=[1,1], UBOUNDS=[11,11], PERSISTENT=.TRUE.)
        CALL O%GET_DEVICE_DATA_RDWR(PTR_DEV)
        !$ACC KERNELS PRESENT(PTR_DEV)
        PTR_DEV=7
        !$ACC END KERNELS

        CALL O%GET_HOST_DATA_RDONLY(PTR_HOST)

        DO I=1,11
        DO J=1,11
          IF(PTR_HOST(I,J)/=7)THEN
                CALL FIELD_ABORT ("ERROR")
          ENDIF
        ENDDO
        ENDDO

        CALL FIELD_DELETE(O)
END PROGRAM GPU_TO_CPU
