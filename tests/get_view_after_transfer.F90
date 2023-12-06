! (C) Copyright 2022- ECMWF.
! (C) Copyright 2022- Meteo-France.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.

PROGRAM GET_VIEW_AFTER_TRANSFER
  ! Test that accessing a previously uninitialised field
  ! on host after it has been written to on device.
  USE FIELD_MODULE
  USE FIELD_FACTORY_MODULE
  USE PARKIND1
  IMPLICIT NONE

  CLASS(FIELD_2RB), POINTER :: F => NULL()
  REAL(KIND=JPRB), POINTER :: PTR_D(:,:), VIEW(:)
  INTEGER :: I, J

  CALL FIELD_NEW(F, UBOUNDS=[10,10], PERSISTENT=.TRUE.)
  CALL F%GET_DEVICE_DATA_RDWR(PTR_D)

  !$ACC KERNELS PRESENT(PTR_D)
  DO I=1,10
    DO J=1,10
      PTR_D(I,J) = 42.0
    ENDDO
  ENDDO
  !$ACC END KERNELS

  CALL F%SYNC_HOST_RDWR()
  DO I=1,10
    VIEW => F%GET_VIEW(I)
    DO J=1,10
      IF (VIEW(J) /= 42.0)  STOP
    ENDDO
  ENDDO

END PROGRAM GET_VIEW_AFTER_TRANSFER
