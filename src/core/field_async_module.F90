! (C) Copyright 2022- ECMWF.
! (C) Copyright 2022- Meteo-France.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.

MODULE FIELD_ASYNC_MODULE

IMPLICIT NONE

CONTAINS

SUBROUTINE WAIT_FOR_ASYNC_QUEUE (QUEUE)

USE PARKIND1, ONLY : JPIM

INTEGER(KIND=JPIM), INTENT(IN) :: QUEUE
!Wait for all data transfer initiated on queue by the current thread
!$acc wait (QUEUE)
END SUBROUTINE WAIT_FOR_ASYNC_QUEUE

END MODULE FIELD_ASYNC_MODULE
