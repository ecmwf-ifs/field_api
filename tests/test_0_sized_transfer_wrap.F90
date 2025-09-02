! (C) Copyright 2022- ECMWF.
! (C) Copyright 2022- Meteo-France.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.

PROGRAM TEST_0_SIZED_TRANSFER
  ! Test that wrapped fields of size 0 are safely transferred to GPU

  USE FIELD_MODULE
  USE FIELD_FACTORY_MODULE
  USE PARKIND1
  USE FIELD_ABORT_MODULE
  USE FIELD_ACCESS_MODULE
  IMPLICIT NONE

  CLASS(FIELD_3RB), POINTER :: W=>NULL()
  REAL(KIND=JPRB), ALLOCATABLE :: D(:,:,:)
  REAL(KIND=JPRB), POINTER :: PTR(:,:,:) => NULL()

  ALLOCATE(D(20, 0, 3))
  CALL FIELD_NEW(W, DATA=D, PERSISTENT=.TRUE.)
  CALL W%GET_HOST_DATA_RDONLY(PTR)

  IF( SIZE(PTR) > 0 )THEN
    CALL FIELD_ABORT("ALLOCATION SHOULD BE 0 SIZED")
  ENDIF

  PTR => GET_HOST_DATA_RDONLY(W)
  IF( SIZE(PTR) /= 1 )THEN
    CALL FIELD_ABORT("ALLOCATION SHOULD BE OF SIZE==1")
  ENDIF

  !...Ensure data can be copied to device and back safely
  CALL W%SYNC_DEVICE_RDWR()
  CALL W%SYNC_HOST_RDWR()

  CALL FIELD_DELETE(W)
END PROGRAM TEST_0_SIZED_TRANSFER
