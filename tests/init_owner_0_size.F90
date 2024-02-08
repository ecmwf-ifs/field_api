! (C) Copyright 2022- ECMWF.
! (C) Copyright 2022- Meteo-France.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.

PROGRAM INIT_OWNER_0_SIZE
        ! TEST IF 0-SIZED ALLOCATION IS HANDLED SAFELY

        USE FIELD_MODULE
        USE FIELD_FACTORY_MODULE
        USE PARKIND1
        USE FIELD_ABORT_MODULE
        IMPLICIT NONE
        CLASS(FIELD_2RB), POINTER :: O => NULL()
        REAL(KIND=JPRB), POINTER :: PTR(:,:)

        CALL FIELD_NEW(O, LBOUNDS=[22,1], UBOUNDS=[21,11], PERSISTENT=.TRUE., DELAYED=.FALSE.)

        IF(.NOT. SIZE(O%PTR) == 0) CALL FIELD_ABORT("ERROR")
 
        CALL O%SYNC_HOST_RDWR

        CALL FIELD_DELETE(O)
END PROGRAM INIT_OWNER_0_SIZE
