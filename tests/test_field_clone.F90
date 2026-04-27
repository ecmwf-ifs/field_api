! (C) Copyright 2022- ECMWF.
! (C) Copyright 2022- Meteo-France.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.

PROGRAM TEST_FIELD_CLONE
  !TEST IF CLONING WORKS CORRECTLY.
  !IT SHOULD CREATE A FIELD_OWNER AND SO LET THE DATA BE
  !ACCESSIBLE EVEN THROUGH THE ORIGIN OF THE COPY HAS BEEN DESTROYED

  USE FIELD_MODULE
  USE FIELD_FACTORY_MODULE
  USE FIELD_CLONE_MODULE, ONLY: FIELD_CLONE_ON_HOST
  USE PARKIND1
  USE FIELD_ABORT_MODULE
  IMPLICIT NONE
  REAL(KIND=JPRB) :: D(10)
  CLASS(FIELD_1RB), POINTER :: W => NULL()
  CLASS(FIELD_1RB), POINTER :: MYCLONE => NULL()

  D = 7
  CALL FIELD_NEW(W, DATA=D)

  CALL FIELD_CLONE_ON_HOST(MYCLONE, W)
  CALL FIELD_DELETE(W)

  IF (.NOT. ALL(MYCLONE%PTR == 7)) THEN
    CALL FIELD_ABORT ("ERROR")
  END IF 

  CALL FIELD_DELETE(MYCLONE)
END PROGRAM TEST_FIELD_CLONE
