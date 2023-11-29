! (C) Copyright 2022- ECMWF.
! (C) Copyright 2022- Meteo-France.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.

PROGRAM TEST_GANG

USE FIELD_MODULE
USE FIELD_FACTORY_MODULE
USE PARKIND1
USE OMP_LIB, ONLY: OMP_GET_MAX_THREADS
IMPLICIT NONE

CLASS(FIELD_4RB), POINTER :: YLF4
TYPE(FIELD_3RB_PTR), ALLOCATABLE :: YLF3L (:)

INTEGER (KIND=JPIM), PARAMETER :: NPROMA = 32, NFLEVG = 15, NDIM = 3, NGPBLKS = 10

CALL FIELD_NEW (YLF4, CHILDREN=YLF3L, UBOUNDS=[NPROMA, NFLEVG, NDIM, NGPBLKS])

IF (SIZE (YLF3L) /= NDIM) ERROR STOP 1

CALL FIELD_DELETE (YLF4)

END PROGRAM 
