! (C) Copyright 2022- ECMWF.
! (C) Copyright 2022- Meteo-France.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.

PROGRAM TEST_0_SIZED_TRANSFER
        ! TEST IF 0-SIZED ALLOCATIONS CAN BE SAFELY TRANSFERRED TO GPU

        USE FIELD_MODULE
        USE FIELD_FACTORY_MODULE
        USE PARKIND1
        USE FIELD_ABORT_MODULE
        USE FIELD_ACCESS_MODULE
        IMPLICIT NONE
        CLASS(FIELD_2RB), POINTER :: O => NULL()
        REAL(KIND=JPRB), POINTER :: PTR(:,:) => NULL()

        CALL FIELD_NEW(O, LBOUNDS=[10,1], UBOUNDS=[9,11], PERSISTENT=.TRUE.)
        PTR => GET_HOST_DATA_RDONLY(O)

        IF( SIZE(O%PTR) > 0 )THEN
          CALL FIELD_ABORT("ALLOCATION SHOULD BE 0 SIZED")
        ENDIF

        !...Ensure data can be copied to device and back safely
        CALL O%SYNC_DEVICE_RDWR()
        CALL O%SYNC_HOST_RDWR()

        CALL FIELD_DELETE(O)
END PROGRAM TEST_0_SIZED_TRANSFER
