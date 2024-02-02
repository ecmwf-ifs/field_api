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
USE FIELD_SHUFFLE_MODULE
USE PARKIND1
USE FIELD_ABORT_MODULE

IMPLICIT NONE

TYPE (FIELD_SHUFFLE):: FGS

INTEGER, PARAMETER :: NPROMA1 = 10, NGPBLKS1 = 4, NPROMA2 = 6, NFLEVG = 3

INTEGER :: JLON, JBLK, JLEV, JPASS

INTEGER (KIND=JPIM), ALLOCATABLE :: D1 (:,:,:)

CLASS (FIELD_3IM), POINTER :: FD => NULL ()

INTEGER (KIND=JPIM), POINTER :: Z2 (:,:,:) => NULL ()

INTEGER (KIND=JPIM) :: FUNC

FUNC (JLON, JBLK) = 1000 * JBLK + JLON

! First pass with modification on CPU, second pass on GPU
DO JPASS = 1, 2

  ALLOCATE (D1 (NPROMA1, NFLEVG, NGPBLKS1))
  
  DO JBLK = 1, NGPBLKS1
    DO JLON = 1, NPROMA1
      D1 (JLON, :, JBLK) = FUNC (JLON, JBLK)
    ENDDO
  ENDDO
  
  DO JBLK = 1, SIZE (D1, 3)
    WRITE (*, '(20I12)') D1 (:, 1, JBLK)
  ENDDO
  
  CALL FIELD_NEW (FD, DATA=D1)
  
  ! Reshuffle on NPROMA2 arrays
  
  CALL FGS%INIT (KGPTOT=NPROMA1*NGPBLKS1, KLON_S=NPROMA1, KLON_G=NPROMA2)
  
  IF (JPASS == 1) THEN
    Z2 => GATHER_HOST_DATA_RDWR (FGS, FD)
  ELSEIF (JPASS == 2) THEN
    Z2 => GATHER_DEVICE_DATA_RDWR (FGS, FD)
  ENDIF
  
  IF (NPROMA2 /= SIZE (Z2, 1)) CALL FIELD_ABORT ('NPROMA MISMATCH')
  
  PRINT *, " NPROMA2 = ", NPROMA2
  PRINT *, " LBOUND (Z2) = ", LBOUND (Z2)
  PRINT *, " UBOUND (Z2) = ", UBOUND (Z2)
  
  DO JBLK = 1, SIZE (Z2, 3)
    WRITE (*, '(20I12)') Z2 (:, 1, JBLK)
  ENDDO
  
  IF (JPASS == 1) THEN
    DO JBLK = 1, SIZE (Z2, 3)
      DO JLEV = 1, NFLEVG
        DO JLON = 1, SIZE (Z2, 1)
          Z2 (JLON, JLEV, JBLK) = (JPASS + 1) * Z2 (JLON, JLEV, JBLK)
        ENDDO
      ENDDO
    ENDDO
  ELSE
!$acc parallel loop gang present (Z2)
    DO JBLK = 1, SIZE (Z2, 3)
!$acc loop vector
      DO JLON = 1, SIZE (Z2, 1)
        DO JLEV = 1, NFLEVG
          Z2 (JLON, JLEV, JBLK) = (JPASS + 1) * Z2 (JLON, JLEV, JBLK)
        ENDDO
      ENDDO
    ENDDO
  ENDIF
  
  ! Reshuffle back to NPROMA1 array
  
  CALL FGS%SCATTER () 
  
  ! Synchronize D1 to host (if data was modified on GPU)
  CALL FIELD_DELETE (FD)

  PRINT *, '------------'

  DO JBLK = 1, SIZE (D1, 3)
    WRITE (*, '(20I12)') D1 (:, 1, JBLK)
  ENDDO
  
  DO JBLK = 1, SIZE (D1, 3)
    DO JLEV = 1, NFLEVG
      DO JLON = 1, NPROMA1
        IF (D1 (JLON, JLEV, JBLK) /= (JPASS + 1) * FUNC (JLON, JBLK)) THEN
          PRINT *, " JPASS = ", JPASS, " JLON = ", JLON, " JLEV = ", JLEV, &
        & " JBLK = ", JBLK, " D1 = ", D1 (JLON, JLEV, JBLK), &
        & (JPASS + 1) * FUNC (JLON, JBLK)
          CALL FIELD_ABORT ('VALUE ERROR')
        ENDIF
      ENDDO
    ENDDO
  ENDDO
  
  DEALLOCATE (D1)
  

ENDDO

END PROGRAM
