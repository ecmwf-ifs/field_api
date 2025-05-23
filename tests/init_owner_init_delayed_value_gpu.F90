! (C) Copyright 2022- ECMWF.
! (C) Copyright 2022- Meteo-France.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.

PROGRAM INIT_OWNER_INIT_DELAYED_VALUE_GPU
        !TEST THAT ALL DATA ARE INITIALIZED TO THE VALUE CHOOSED BY THE USER
        !WHEN COMBINED WITH DELAYED ALLOCATION ON GPU
        USE FIELD_MODULE
        USE FIELD_FACTORY_MODULE
        USE PARKIND1
        USE FIELD_ABORT_MODULE
        IMPLICIT NONE
        CLASS(FIELD_2IM), POINTER :: O => NULL()
        INTEGER(KIND=JPIM), POINTER :: PTR(:,:)
        LOGICAL :: OKAY

        CALL FIELD_NEW(O, LBOUNDS=[1,1], UBOUNDS=[10,10], INIT_VALUE=7, DELAYED=.TRUE.)
        CALL O%GET_DEVICE_DATA_RDONLY(PTR)

        OKAY=.TRUE.
#ifdef OMPGPU
        !$OMP TARGET MAP(TO:PTR) MAP(TOFROM:OKAY)
#else
        !$ACC SERIAL PRESENT(PTR) COPY(OKAY)
#endif
        IF(.NOT. ALL(PTR == 7))THEN
                OKAY=.FALSE.
        ENDIF
#ifdef OMPGPU
        !$OMP END TARGET
#else
        !$ACC END SERIAL
#endif

        IF(OKAY .EQV. .FALSE.)THEN
                CALL FIELD_ABORT ("ERROR")
        ENDIF

        CALL FIELD_DELETE(O)
END PROGRAM INIT_OWNER_INIT_DELAYED_VALUE_GPU
