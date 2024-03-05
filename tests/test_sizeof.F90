! (C) Copyright 2022- ECMWF.
! (C) Copyright 2022- Meteo-France.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.

PROGRAM TEST_SIZEOF

USE FIELD_MODULE
USE FIELD_FACTORY_MODULE
USE FIELD_ACCESS_MODULE
USE PARKIND1
USE FIELD_ABORT_MODULE
IMPLICIT NONE

CLASS(FIELD_4RB), POINTER :: YLFA => NULL()
CLASS(FIELD_4RB), POINTER :: YLFB => NULL()

REAL(KIND=JPRB), TARGET, ALLOCATABLE :: ZDATA (:,:,:,:)

REAL(KIND=JPRB), POINTER :: ZFLDA (:,:,:,:) => NULL ()
REAL(KIND=JPRB), POINTER :: ZFLDB (:,:,:,:) => NULL ()

REAL(KIND=JPRB), POINTER :: ZHST (:,:,:,:) => NULL ()
REAL(KIND=JPRB), POINTER :: ZDEV (:,:,:,:) => NULL ()

ALLOCATE(ZDATA (4, 10, 5, 6))
ZDATA = 0

ZFLDA => ZDATA (1:2, :, :, :)
ZFLDB => ZDATA (3:4, :, :, :)

ZFLDA = 123.
ZFLDB = 456.

CALL FIELD_NEW (YLFA, DATA=ZFLDA)
CALL FIELD_NEW (YLFB, DATA=ZFLDB)

ZDEV => GET_DEVICE_DATA_RDWR (YLFA)
ZHST => GET_HOST_DATA_RDONLY (YLFA)

ZHST => GET_HOST_DATA_RDONLY (YLFB)

IF (.NOT. ALL(ZHST == 456.)) THEN
  CALL FIELD_ABORT ("ERROR: UNEXPECTED VALUES")
END IF 

CALL FIELD_DELETE (YLFA)
CALL FIELD_DELETE (YLFB)

END PROGRAM 
