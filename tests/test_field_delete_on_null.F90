! (C) Copyright 2022- ECMWF.
! (C) Copyright 2022- Meteo-France.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.

PROGRAM TEST_FIELD_DELETE_ON_NULL
        !TEST IF CALLING FIELD_DELETE WITH A NULL POINTER IS WORKING AS IT SHOULD

        USE FIELD_MODULE
        USE FIELD_FACTORY_MODULE
        IMPLICIT NONE
        CLASS(FIELD_1RB), POINTER :: W => NULL()
        CALL FIELD_DELETE(W)
END PROGRAM 
