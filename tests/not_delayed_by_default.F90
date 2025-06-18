! (C) Copyright 2022- ECMWF.
! (C) Copyright 2022- Meteo-France.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.

PROGRAM NOT_DELAYED_BY_DEFAULT
        ! TEST THAT SETTING THE DEFAULT DELAYED VALUE TO FALSE IS WORKING

        USE FIELD_FACTORY_MODULE
        USE FIELD_MODULE
        USE PARKIND1
        USE FIELD_ABORT_MODULE
        USE FIELD_DEFAULTS_MODULE, ONLY: DELAYED_DEFAULT_VALUE
        IMPLICIT NONE
        CLASS(FIELD_2RB), POINTER :: O => NULL()

        DELAYED_DEFAULT_VALUE = .FALSE.
        CALL FIELD_NEW(O, LBOUNDS=[1,1], UBOUNDS=[2,11])

        IF (.NOT. ASSOCIATED(O%PTR)) THEN
          CALL FIELD_ABORT ("ERROR, POINTER TO DATA ON CPU SHOULD HAVE BEEN ASSOCIATED")
        END IF 
        CALL FIELD_DELETE(O)
END PROGRAM NOT_DELAYED_BY_DEFAULT
