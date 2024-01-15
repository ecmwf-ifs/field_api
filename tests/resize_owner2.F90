! (C) Copyright 2022- ECMWF.
! (C) Copyright 2022- Meteo-France.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.

PROGRAM RESIZE_OWNER
        !TEST IF RESIZING A OWNER IS WORKING WHEN SIZE IS REDUCED

        USE FIELD_MODULE
        USE FIELD_FACTORY_MODULE
        USE PARKIND1
        USE FIELD_ABORT_MODULE
        IMPLICIT NONE
        CLASS(FIELD_2RB), POINTER :: O => NULL()
        REAL(KIND=JPRB), POINTER :: PTR(:,:)
        INTEGER(KIND=JPIM) :: L(2),U(2)

        CALL FIELD_NEW(O, LBOUNDS=[10,1], UBOUNDS=[21,11], PERSISTENT=.TRUE.)
        CALL O%GET_HOST_DATA_RDWR(PTR)
        PTR=42

        CALL FIELD_RESIZE(O, UBOUNDS=[1,1], PERSISTENT=.TRUE.)
        CALL O%GET_HOST_DATA_RDWR(PTR)
        PTR=7

        CALL O%GET_DIMS(UBOUNDS=U,LBOUNDS=L)
        IF(.NOT. U(1) == 1)THEN
                WRITE(*,*)"U(1) != 1"
                CALL FIELD_ABORT ("ERROR")
        END IF
        IF(.NOT. U(2) == 1)THEN
                WRITE(*,*)"U(2) != 1"
                CALL FIELD_ABORT ("ERROR")
        END IF
        IF (.NOT. ALL(PTR == 7)) THEN
                WRITE(*,*)"PTR != 7"
                CALL FIELD_ABORT ("ERROR")
        END IF 
        CALL FIELD_DELETE(O)
END PROGRAM RESIZE_OWNER
