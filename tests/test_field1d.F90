! (C) Copyright 2022- ECMWF.
! (C) Copyright 2022- Meteo-France.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.

PROGRAM TEST_FIELD1D
        ! TEST IF WRAPPER IS REALLY WRAPPING DATA
        ! IF IT IS WRAPPING THEN MODIFYING THE WRAPED DATA SHOULD BE SEEN IN
        ! WRAPPER TOO

        USE FIELD_MODULE
        USE FIELD_FACTORY_MODULE
        USE FIELD_ACCESS_MODULE
        USE PARKIND1
        USE FIELD_ABORT_MODULE
        IMPLICIT NONE
        CLASS(FIELD_1RB), POINTER :: W => NULL()
        REAL(KIND=JPRB), ALLOCATABLE :: D(:)
        REAL(KIND=JPRB), POINTER :: DD(:)

        ALLOCATE(D(10))
        D=7

        CALL FIELD_NEW(W, DATA=D)
        D=42

        IF (.NOT. ALL(W%PTR == 42)) THEN
          CALL FIELD_ABORT ("ERROR")
        END IF 


        DD => GET_DEVICE_DATA_RDWR (W)

!$acc serial present (DD)
        DD = 22
!$acc end serial

        DD => GET_HOST_DATA_RDONLY (W)

        IF (.NOT. ALL(DD == 22)) THEN
          CALL FIELD_ABORT ("ERROR")
        END IF 

        CALL FIELD_DELETE(W)
END PROGRAM 
