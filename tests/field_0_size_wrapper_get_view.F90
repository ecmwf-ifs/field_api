! (C) Copyright 2022- ECMWF.
! (C) Copyright 2022- Meteo-France.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.

PROGRAM FIELD_0_SIZE_WRAPPER_GET_VIEW
  ! Test that 0-sized field wrapper works with get_view

  USE FIELD_MODULE
  USE FIELD_FACTORY_MODULE
  USE PARKIND1
  USE FIELD_ABORT_MODULE
  USE FIELD_ACCESS_MODULE
  IMPLICIT NONE

  CLASS(FIELD_3RB), POINTER :: W=>NULL()
  REAL(KIND=JPRB), ALLOCATABLE :: D(:,:,:)
  REAL(KIND=JPRB), POINTER :: VIEW(:,:) => NULL()

  ALLOCATE(D(20, 0, 3))
  CALL FIELD_NEW(W, DATA=D, PERSISTENT=.TRUE.)
  WRITE(*,*)"STATUS:",W%GET_STATUS()
  WRITE(*,*)"ASSOCIATED:",ASSOCIATED(W%PTR)
  WRITE(*,*)"SIZE:",SIZE(W%PTR)

  VIEW=>W%GET_VIEW(1)
  WRITE(*,*)"VIEW SIZE:",SIZE(VIEW)
  WRITE(*,*)"VIEW CONTENT:",VIEW

  IF( SIZE(W%PTR) > 0 )THEN
    CALL FIELD_ABORT("ALLOCATION SHOULD BE 0 SIZED")
  ENDIF

  CALL FIELD_DELETE(W)
END PROGRAM FIELD_0_SIZE_WRAPPER_GET_VIEW
