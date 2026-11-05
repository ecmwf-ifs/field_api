! (C) Copyright 2022- ECMWF.
! (C) Copyright 2022- Meteo-France.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.

! Review finding 3 (PR #163): after a member was updated on device,
! STACK%GET_VIEW returns stale host data, and the member's device result is
! lost for good: neither a host nor a device retrieval of the member returns it.
!
! Black-box test: only the public API is used, and only data returned by
! retrieval calls is compared with the value last written (field_read_check.inc).
! GET_VIEW_ABORT is switched off at run time so that GET_VIEW performs the D2H
! sync itself (with it on, the correct behaviour would be an abort).

PROGRAM TEST_FIELD_STACK_GET_VIEW_STALE
    USE FIELD_FACTORY_MODULE
    USE FIELD_MODULE
    USE FIELD_ABORT_MODULE
    USE FIELD_DEFAULTS_MODULE, ONLY: GET_VIEW_ABORT
    USE PARKIND1, ONLY: JPRB
    USE EC_PARKIND, ONLY: JPIM

    IMPLICIT NONE

    INTEGER(KIND=JPIM), PARAMETER :: NPROMA = 8, NBLKS = 4
    CLASS(FIELD_3RB), POINTER :: F_STACK => NULL()
    CLASS(FIELD_2RB), POINTER :: F_A => NULL(), F_B => NULL()
    REAL(KIND=JPRB), POINTER :: PTR(:,:,:), DA(:,:), HA(:,:), HB(:,:), VIEW(:,:)
    INTEGER(KIND=JPIM) :: NERR

    NERR = 0
    GET_VIEW_ABORT = .FALSE.

    WRITE (*, '(/,A)') '--- scenario 3: member updated on device, then STACK%GET_VIEW ---'
    CALL FIELD_NEW (F_STACK, UBOUNDS=[NPROMA, 2, NBLKS], LSTACK=.TRUE., &
    &               MEMBER_MAP=[1, 1, 2, 2], PERSISTENT=.TRUE.)
    CALL F_STACK%GET_HOST_DATA_RDWR (PTR)
    PTR = 0._JPRB
    CALL GET_STACK_MEMBER (F_STACK, 1, F_A)
    CALL GET_STACK_MEMBER (F_STACK, 2, F_B)
    CALL STEP ('STACK%GET_HOST_DATA_RDWR, fill A=0, B=0')

    CALL F_A%GET_DEVICE_DATA_RDWR (DA)
    CALL FILL_DEVICE_2D (DA, 1._JPRB)
    CALL STEP ('A%GET_DEVICE_DATA_RDWR, kernel writes A=1')

    VIEW => F_STACK%GET_VIEW (1)                      ! host view of block 1: (NPROMA, 2)
    CALL CHECK_READ ('STACK%GET_VIEW(1), A column', VIEW(:,1:1), 1._JPRB, NERR)
    CALL CHECK_READ ('STACK%GET_VIEW(1), B column', VIEW(:,2:2), 0._JPRB, NERR)

    CALL F_A%GET_HOST_DATA_RDONLY (HA)
    CALL CHECK_READ ('A%GET_HOST_DATA_RDONLY', HA, 1._JPRB, NERR)
    CALL F_A%GET_DEVICE_DATA_RDONLY (DA)
    CALL CHECK_DEVICE_READ ('A%GET_DEVICE_DATA_RDONLY', DA, 1._JPRB, NERR)
    CALL F_B%GET_HOST_DATA_RDONLY (HB)
    CALL CHECK_READ ('B%GET_HOST_DATA_RDONLY', HB, 0._JPRB, NERR)

    CALL FIELD_DELETE (F_STACK)

    WRITE (*, '(/,"spurious reads: ",I0)') NERR
    IF (NERR > 0) CALL FIELD_ABORT ('TEST_FIELD_STACK_GET_VIEW_STALE FAILED')

CONTAINS

#include "field_read_check.inc"

END PROGRAM TEST_FIELD_STACK_GET_VIEW_STALE
