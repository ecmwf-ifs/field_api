! (C) Copyright 2022- ECMWF.
! (C) Copyright 2022- Meteo-France.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.

PROGRAM TEST_FIELD_ARRAY_WRAPPER
  ! Test FIELD_ARRAY wrapper with 0-sized allocatables

  USE FIELD_ARRAY_MODULE
  USE PARKIND1
  USE FIELD_ABORT_MODULE
  IMPLICIT NONE
  TYPE(FIELD_2RB_ARRAY) :: W
  REAL(KIND=JPRB), ALLOCATABLE :: D(:,:)

  IF(ASSOCIATED(W%F_P)) CALL FIELD_ABORT ("FIELD_ARRAY SHOULD REMAIN UNINITIALISED")

  ALLOCATE(D(10,0))
  CALL W%INIT(D)

  ! Check that the FIELD_ARRAY has been created correctly
  IF (.NOT. ASSOCIATED(W%F_P))  CALL FIELD_ABORT ("ERROR: Field not created")
  IF (.NOT. ASSOCIATED(W%F_P%PTR))  CALL FIELD_ABORT ("ERROR: Field has no PTR")
  IF (SIZE(W%F_P%PTR) /= 0)  CALL FIELD_ABORT ("ERROR: Field has wrong size")

  CALL W%FINAL()
END PROGRAM TEST_FIELD_ARRAY_WRAPPER
