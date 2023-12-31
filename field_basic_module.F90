! (C) Copyright 2022- ECMWF.
! (C) Copyright 2022- Meteo-France.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.

MODULE FIELD_BASIC_MODULE

USE DEV_ALLOC_MODULE
USE PARKIND1, ONLY : JPIM

IMPLICIT NONE

PRIVATE

TYPE GPU_STATS
  INTEGER :: TRANSFER_CPU_TO_GPU = 0
  INTEGER :: TRANSFER_GPU_TO_CPU = 0
  REAL :: TOTAL_TIME_TRANSFER_CPU_TO_GPU = 0
  REAL :: TOTAL_TIME_TRANSFER_GPU_TO_CPU = 0
  CONTAINS
  PROCEDURE :: INC_CPU_TO_GPU_TRANSFER
  PROCEDURE :: INC_GPU_TO_CPU_TRANSFER
END TYPE GPU_STATS

TYPE, ABSTRACT :: FIELD_BASIC
  LOGICAL :: THREAD_BUFFER = .FALSE.

  INTEGER(KIND=JPIM) :: ISTATUS = 0
  INTEGER(KIND=JPIM) :: LAST_CONTIGUOUS_DIMENSION = 0

  TYPE(GPU_STATS) :: STATS

  LOGICAL :: LOBJECT_COPIED = .FALSE.

CONTAINS
  PROCEDURE (FIELD_BASIC_SYNC), DEFERRED :: SYNC_HOST_RDWR
  PROCEDURE (FIELD_BASIC_SYNC), DEFERRED :: SYNC_HOST_RDONLY
  PROCEDURE (FIELD_BASIC_SYNC), DEFERRED :: SYNC_DEVICE_RDWR
  PROCEDURE (FIELD_BASIC_SYNC), DEFERRED :: SYNC_DEVICE_RDONLY
END TYPE FIELD_BASIC

PUBLIC :: FIELD_BASIC

ABSTRACT INTERFACE
  SUBROUTINE FIELD_BASIC_SYNC (SELF, QUEUE)
    IMPORT FIELD_BASIC
    CLASS(FIELD_BASIC), INTENT(INOUT) :: SELF
    INTEGER, OPTIONAL,  INTENT(IN)    :: QUEUE
  END SUBROUTINE
END INTERFACE

PUBLIC :: FIELD_BASIC_SYNC

CONTAINS

SUBROUTINE INC_CPU_TO_GPU_TRANSFER(SELF, START, FINISH)
  CLASS(GPU_STATS), INTENT(INOUT) :: SELF
  REAL, INTENT(IN) :: START, FINISH
  SELF%TRANSFER_CPU_TO_GPU = SELF%TRANSFER_CPU_TO_GPU + 1
  SELF%TOTAL_TIME_TRANSFER_CPU_TO_GPU = SELF%TOTAL_TIME_TRANSFER_CPU_TO_GPU + FINISH - START
END SUBROUTINE

SUBROUTINE INC_GPU_TO_CPU_TRANSFER(SELF, START, FINISH)
  CLASS(GPU_STATS), INTENT(INOUT) :: SELF
  REAL, INTENT(IN) :: START, FINISH
  SELF%TRANSFER_GPU_TO_CPU = SELF%TRANSFER_GPU_TO_CPU + 1
  SELF%TOTAL_TIME_TRANSFER_GPU_TO_CPU = SELF%TOTAL_TIME_TRANSFER_GPU_TO_CPU + FINISH - START
END SUBROUTINE

END MODULE FIELD_BASIC_MODULE
