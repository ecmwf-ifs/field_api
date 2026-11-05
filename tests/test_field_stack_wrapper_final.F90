! (C) Copyright 2022- ECMWF.
! (C) Copyright 2022- Meteo-France.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.

! Review finding 4 (PR #163), revised: SYNC_ON_FINAL on a wrapper stack.
! - Scenario 4a (whole stack written on device) passes: the wrapper FINAL does a
!   plain whole-buffer D2H copy (the originally suspected use-after-free through
!   the new stack GET_HOST_DATA does not happen).
! - Scenario 4b (a single member written on device) FAILS on PR #163: the
!   wrapper FINAL decides on the D2H copy from the stack's own status, which
!   still says host-fresh because only the member moved, so the member's device
!   result never reaches the user's array. Same root cause as findings 1 and 3.
!
! Black-box test: only the public API is used, and only data obtained back
! (here: the user's wrapped array after FIELD_DELETE) is compared with the value
! last written (field_read_check.inc). Two variants: whole stack written on
! device, and a single member written on device.

PROGRAM TEST_FIELD_STACK_WRAPPER_FINAL
    USE FIELD_FACTORY_MODULE
    USE FIELD_MODULE
    USE FIELD_ABORT_MODULE
    USE PARKIND1, ONLY: JPRB
    USE EC_PARKIND, ONLY: JPIM

    IMPLICIT NONE

    INTEGER(KIND=JPIM), PARAMETER :: NPROMA = 8, NBLKS = 4
    REAL(KIND=JPRB), ALLOCATABLE, TARGET :: BUFFER(:,:,:)
    CLASS(FIELD_3RB), POINTER :: F_STACK => NULL()
    CLASS(FIELD_2RB), POINTER :: F_A => NULL()
    REAL(KIND=JPRB), POINTER :: DPTR(:,:,:), DA(:,:)
    INTEGER(KIND=JPIM) :: NERR

    NERR = 0
    ALLOCATE (BUFFER(NPROMA, 2, NBLKS))

    !---------------------------------------------------------------------------
    WRITE (*, '(/,A)') '--- scenario 4a: whole wrapper stack written on device, then FIELD_DELETE ---'
    BUFFER = 0._JPRB
    CALL FIELD_NEW (F_STACK, DATA=BUFFER, LSTACK=.TRUE., MEMBER_MAP=[1, 1, 2, 2])   ! SYNC_ON_FINAL defaults to .TRUE.
    CALL STEP ('FIELD_NEW (wrapper stack on BUFFER = 0)')

    CALL F_STACK%GET_DEVICE_DATA_RDWR (DPTR)
    CALL FILL_DEVICE_3D (DPTR, 4._JPRB)
    CALL STEP ('STACK%GET_DEVICE_DATA_RDWR, kernel writes A=4, B=4')

    CALL FIELD_DELETE (F_STACK)
    CALL STEP ('FIELD_DELETE (SYNC_ON_FINAL)')
    CALL CHECK_READ ('BUFFER(:,1,:) (A)', BUFFER(:,1,:), 4._JPRB, NERR)
    CALL CHECK_READ ('BUFFER(:,2,:) (B)', BUFFER(:,2,:), 4._JPRB, NERR)

    !---------------------------------------------------------------------------
    WRITE (*, '(/,A)') '--- scenario 4b: one member of a wrapper stack written on device, then FIELD_DELETE ---'
    BUFFER = 0._JPRB
    CALL FIELD_NEW (F_STACK, DATA=BUFFER, LSTACK=.TRUE., MEMBER_MAP=[1, 1, 2, 2])
    CALL GET_STACK_MEMBER (F_STACK, 1, F_A)
    CALL STEP ('FIELD_NEW (wrapper stack on BUFFER = 0)')

    CALL F_A%GET_DEVICE_DATA_RDWR (DA)
    CALL FILL_DEVICE_2D (DA, 6._JPRB)
    CALL STEP ('A%GET_DEVICE_DATA_RDWR, kernel writes A=6')

    NULLIFY (F_A)
    CALL FIELD_DELETE (F_STACK)
    CALL STEP ('FIELD_DELETE (SYNC_ON_FINAL)')
    CALL CHECK_READ ('BUFFER(:,1,:) (A)', BUFFER(:,1,:), 6._JPRB, NERR)
    CALL CHECK_READ ('BUFFER(:,2,:) (B)', BUFFER(:,2,:), 0._JPRB, NERR)

    WRITE (*, '(/,"spurious reads: ",I0)') NERR
    IF (NERR > 0) CALL FIELD_ABORT ('TEST_FIELD_STACK_WRAPPER_FINAL FAILED')

CONTAINS

#include "field_read_check.inc"

END PROGRAM TEST_FIELD_STACK_WRAPPER_FINAL
