! (C) Copyright 2022- ECMWF.
! (C) Copyright 2022- Meteo-France.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.

PROGRAM TEST_FIELD_CLONE_OPTIONAL
  !TEST IF CLONING WORKS CORRECTLY.
  !THE CLONE SHOULD BE NULL IS THE SOURCE OF THE COPY IS NOT PRESENT

  USE FIELD_MODULE
  USE FIELD_FACTORY_MODULE
  USE FIELD_CLONE_MODULE, ONLY: FIELD_CLONE_ON_HOST
  USE PARKIND1
  USE FIELD_ABORT_MODULE
  IMPLICIT NONE
  CLASS(FIELD_1RB), POINTER :: MYCLONE => NULL()

  CALL FIELD_CLONE_ON_HOST(MYCLONE)

  IF (ASSOCIATED(MYCLONE)) THEN
    CALL FIELD_ABORT ("ERROR")
  END IF 

  CALL FIELD_DELETE(MYCLONE)
END PROGRAM TEST_FIELD_CLONE_OPTIONAL
