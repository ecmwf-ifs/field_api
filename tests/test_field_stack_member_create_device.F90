! (C) Copyright 2022- ECMWF.
! (C) Copyright 2022- Meteo-France.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.

! Regression guard for review finding 2 (PR #163) -- finding REFUTED, test passes.
! Suspected: when member B triggers the first device allocation of the stack,
! member A (already initialised on host through its own pointer) would be reset
! to UNINITIALIZED and its value never sent to the device.
! Actual: the allocation status change does not propagate to the members.
! Kept to make sure a fix for findings 1/3 does not break this sequence.
!
! Black-box test: only the public API is used, and only data returned by
! retrieval calls is compared with the value last written (field_read_check.inc).
! The whole stack is never accessed; each member is filled through its own pointer.

PROGRAM TEST_FIELD_STACK_MEMBER_CREATE_DEVICE
    USE FIELD_FACTORY_MODULE
    USE FIELD_MODULE
    USE FIELD_ABORT_MODULE
    USE PARKIND1, ONLY: JPRB
    USE EC_PARKIND, ONLY: JPIM

    IMPLICIT NONE

    INTEGER(KIND=JPIM), PARAMETER :: NPROMA = 8, NBLKS = 4
    CLASS(FIELD_3RB), POINTER :: F_STACK => NULL()
    CLASS(FIELD_2RB), POINTER :: F_A => NULL(), F_B => NULL()
    REAL(KIND=JPRB), POINTER :: HA(:,:), DA(:,:), HB(:,:), DB(:,:)
    INTEGER(KIND=JPIM) :: NERR

    NERR = 0

    WRITE (*, '(/,A)') '--- scenario 2: members initialised separately, first device access through B ---'
    CALL FIELD_NEW (F_STACK, UBOUNDS=[NPROMA, 2, NBLKS], LSTACK=.TRUE., &
    &               MEMBER_MAP=[1, 1, 2, 2], PERSISTENT=.TRUE.)
    CALL GET_STACK_MEMBER (F_STACK, 1, F_A)
    CALL GET_STACK_MEMBER (F_STACK, 2, F_B)
    CALL STEP ('FIELD_NEW (owner stack, nothing initialised)')

    CALL F_A%GET_HOST_DATA_RDWR (HA)
    HA = 7._JPRB
    CALL STEP ('A%GET_HOST_DATA_RDWR, host writes A=7')

    CALL F_B%GET_DEVICE_DATA_RDWR (DB)
    CALL FILL_DEVICE_2D (DB, 8._JPRB)
    CALL STEP ('B%GET_DEVICE_DATA_RDWR (first device allocation), kernel writes B=8')

    CALL F_A%GET_DEVICE_DATA_RDONLY (DA)
    CALL CHECK_DEVICE_READ ('A%GET_DEVICE_DATA_RDONLY', DA, 7._JPRB, NERR)
    CALL F_B%GET_HOST_DATA_RDONLY (HB)
    CALL CHECK_READ ('B%GET_HOST_DATA_RDONLY', HB, 8._JPRB, NERR)
    CALL F_A%GET_HOST_DATA_RDONLY (HA)
    CALL CHECK_READ ('A%GET_HOST_DATA_RDONLY', HA, 7._JPRB, NERR)

    CALL FIELD_DELETE (F_STACK)

    WRITE (*, '(/,"spurious reads: ",I0)') NERR
    IF (NERR > 0) CALL FIELD_ABORT ('TEST_FIELD_STACK_MEMBER_CREATE_DEVICE FAILED')

CONTAINS

#include "field_read_check.inc"

END PROGRAM TEST_FIELD_STACK_MEMBER_CREATE_DEVICE
