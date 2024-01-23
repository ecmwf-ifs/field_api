! (C) Copyright 2022- ECMWF.
! (C) Copyright 2022- Meteo-France.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.

PROGRAM RESHUFFLE

USE FIELD_MODULE
USE FIELD_FACTORY_MODULE
USE FIELD_ACCESS_MODULE
USE FIELD_GATHSCAT_MODULE
USE PARKIND1
USE FIELD_ABORT_MODULE

IMPLICIT NONE

TYPE (FIELD_GATHSCAT):: FGS

INTEGER, PARAMETER :: NPROMA1 = 10, NGPBLKS1 = 4, NPROMA2 = 6

INTEGER :: I, J

INTEGER (KIND=JPIM), ALLOCATABLE :: D1 (:,:)

CLASS (FIELD_2IM), POINTER :: FD => NULL ()

INTEGER (KIND=JPIM), POINTER :: Z2 (:,:) => NULL ()

INTEGER (KIND=JPIM) :: FUNC

FUNC (I, J) = 1000 * J + I

ALLOCATE (D1 (NPROMA1, NGPBLKS1))

DO J = 1, NGPBLKS1
  DO I = 1, NPROMA1
    D1 (I, J) = FUNC (I, J)
  ENDDO
ENDDO

DO J = 1, SIZE (D1, 2)
  WRITE (*, '(20I12)') D1 (:, J)
ENDDO

CALL FIELD_NEW (FD, DATA=D1)

! Reshuffle on NPROMA2 arrays

CALL FGS%INIT (KGPTOT=NPROMA1*NGPBLKS1, KLON_S=NPROMA1, KLON_G=NPROMA2)

Z2 => GATHER_HOST_DATA_RDWR (FGS, FD)

IF (NPROMA2 /= SIZE (Z2, 1)) CALL FIELD_ABORT ('NPROMA MISMATCH')

PRINT *, " NPROMA2 = ", NPROMA2
PRINT *, " LBOUND (Z2) = ", LBOUND (Z2)
PRINT *, " UBOUND (Z2) = ", UBOUND (Z2)

DO J = 1, SIZE (Z2, 2)
  WRITE (*, '(20I12)') Z2 (:, J)
ENDDO

PRINT *, '------------'

! Reshuffle back to NPROMA1 array

CALL FGS%SCATTER () 

DO J = 1, SIZE (D1, 2)
  WRITE (*, '(20I12)') D1 (:, J)
ENDDO

DO J = 1, SIZE (D1, 2)
  DO I = 1, NPROMA1
    IF (D1 (I, J) /= FUNC (I, J)) THEN
      CALL FIELD_ABORT ('VALUE ERROR')
    ENDIF
  ENDDO
ENDDO

END PROGRAM
