! (C) Copyright 2022- ECMWF.
! (C) Copyright 2022- Meteo-France.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.

PROGRAM RESIZE_WRAPPER
        !TEST IF RESIZING A WRAPPER IS FAILING AS IT SHOULD

        USE FIELD_MODULE
        USE FIELD_FACTORY_MODULE
        USE PARKIND1
        IMPLICIT NONE
        CLASS(FIELD_2RB), POINTER :: W => NULL()
        REAL(KIND=JPRB) :: D(5,5)

        CALL FIELD_NEW(W, DATA=D)
        CALL FIELD_RESIZE(W, UBOUNDS=[100,100])

        CALL FIELD_DELETE(W)
END PROGRAM RESIZE_WRAPPER
