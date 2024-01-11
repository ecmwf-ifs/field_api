! (C) Copyright 2022- ECMWF.
! (C) Copyright 2022- Meteo-France.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.

PROGRAM TEST_PINNED
USE FIELD_MODULE
USE FIELD_FACTORY_MODULE
USE FIELD_UTIL_MODULE
USE OMP_LIB
USE PARKIND1

CLASS(FIELD_2RB), POINTER :: W => NULL()
INTEGER :: NPROMA = 24
INTEGER :: NBLOCKS= 100

DO I = 1, 10
CALL FIELD_NEW(W, UBOUNDS=[NPROMA,10,NBLOCKS])
CALL FIELD_DELETE(W)
ENDDO

END PROGRAM 
