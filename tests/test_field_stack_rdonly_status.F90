! (C) Copyright 2022- ECMWF.
! (C) Copyright 2022- Meteo-France.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.

! Review finding 1 (PR #163): a stack-level RDONLY sync, issued after one member
! was updated on its own, makes a later retrieval of that member return stale data.
!
! Black-box test: only the public API is used, and only data returned by
! retrieval calls is compared with the value last written (field_read_check.inc).
! Stack: FIELD_3RB (NPROMA, 2, NBLKS), rank-2 members A (level 1) and B (level 2).
! On a host-only build, writing through a device pointer simulates a kernel.

PROGRAM TEST_FIELD_STACK_RDONLY_STATUS
    USE FIELD_FACTORY_MODULE
    USE FIELD_MODULE
    USE FIELD_ABORT_MODULE
    USE PARKIND1, ONLY: JPRB
    USE EC_PARKIND, ONLY: JPIM

    IMPLICIT NONE

    INTEGER(KIND=JPIM), PARAMETER :: NPROMA = 8, NBLKS = 4
    CLASS(FIELD_3RB), POINTER :: F_STACK => NULL()
    CLASS(FIELD_2RB), POINTER :: F_A => NULL(), F_B => NULL()
    REAL(KIND=JPRB), POINTER :: PTR(:,:,:), HA(:,:), DA(:,:), HB(:,:), DB(:,:)
    INTEGER(KIND=JPIM) :: NERR

    NERR = 0

    !---------------------------------------------------------------------------
    WRITE (*, '(/,A)') '--- scenario 1a: member updated on device, then STACK%SYNC_DEVICE_RDONLY ---'
    CALL MAKE_STACK ()
    CALL STEP ('STACK%GET_HOST_DATA_RDWR, fill A=0, B=0')

    CALL F_A%GET_DEVICE_DATA_RDWR (DA)
    CALL FILL_DEVICE_2D (DA, 1._JPRB)
    CALL STEP ('A%GET_DEVICE_DATA_RDWR, kernel writes A=1')

    CALL F_STACK%SYNC_DEVICE_RDONLY ()
    CALL STEP ('STACK%SYNC_DEVICE_RDONLY')

    CALL F_A%GET_HOST_DATA_RDONLY (HA)
    CALL CHECK_READ ('A%GET_HOST_DATA_RDONLY', HA, 1._JPRB, NERR)
    CALL F_B%GET_HOST_DATA_RDONLY (HB)
    CALL CHECK_READ ('B%GET_HOST_DATA_RDONLY', HB, 0._JPRB, NERR)

    CALL FIELD_DELETE (F_STACK)

    !---------------------------------------------------------------------------
    WRITE (*, '(/,A)') '--- scenario 1b: member updated on host, then STACK%SYNC_HOST_RDONLY ---'
    CALL MAKE_STACK ()
    CALL STEP ('STACK%GET_HOST_DATA_RDWR, fill A=0, B=0')

    CALL F_STACK%SYNC_DEVICE_RDWR ()
    CALL STEP ('STACK%SYNC_DEVICE_RDWR')

    CALL F_A%GET_HOST_DATA_RDWR (HA)
    HA = 5._JPRB
    CALL STEP ('A%GET_HOST_DATA_RDWR, host writes A=5')

    CALL F_STACK%SYNC_HOST_RDONLY ()
    CALL STEP ('STACK%SYNC_HOST_RDONLY')

    CALL F_A%GET_DEVICE_DATA_RDONLY (DA)
    CALL CHECK_DEVICE_READ ('A%GET_DEVICE_DATA_RDONLY', DA, 5._JPRB, NERR)
    CALL F_B%GET_DEVICE_DATA_RDONLY (DB)
    CALL CHECK_DEVICE_READ ('B%GET_DEVICE_DATA_RDONLY', DB, 0._JPRB, NERR)

    CALL FIELD_DELETE (F_STACK)

    WRITE (*, '(/,"spurious reads: ",I0)') NERR
    IF (NERR > 0) CALL FIELD_ABORT ('TEST_FIELD_STACK_RDONLY_STATUS FAILED')

CONTAINS

    SUBROUTINE MAKE_STACK ()
      CALL FIELD_NEW (F_STACK, UBOUNDS=[NPROMA, 2, NBLKS], LSTACK=.TRUE., &
      &               MEMBER_MAP=[1, 1, 2, 2], PERSISTENT=.TRUE.)
      CALL F_STACK%GET_HOST_DATA_RDWR (PTR)
      PTR = 0._JPRB
      CALL GET_STACK_MEMBER (F_STACK, 1, F_A)
      CALL GET_STACK_MEMBER (F_STACK, 2, F_B)
    END SUBROUTINE MAKE_STACK

#include "field_read_check.inc"

END PROGRAM TEST_FIELD_STACK_RDONLY_STATUS
