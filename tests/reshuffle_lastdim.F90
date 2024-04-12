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

INTEGER, PARAMETER :: NPROMA1 = 10, JBLK1A = 2, JBLK1B = 5, NGPBLKS1 = JBLK1B - JBLK1A + 1
INTEGER, PARAMETER :: NPROMA2 =  6
INTEGER, PARAMETER :: NFLEVG = 3

INTEGER :: JLON, JBLK, JLEV, JPASS, JBOFF
INTEGER :: JBLK1L, JBLK1U

INTEGER (KIND=JPIM), ALLOCATABLE :: D1 (:,:,:)

CLASS (FIELD_3IM), POINTER :: FD => NULL ()

INTEGER (KIND=JPIM), POINTER :: Z2 (:,:,:) => NULL ()

INTEGER (KIND=JPIM) :: FUNC

FUNC (JLON, JBLK) = 1000 * JBLK + JLON

DO JBOFF = 1, 2
! First pass with modification on CPU, second pass on GPU
DO JPASS = 1, 2

  PRINT *, "=========> JPASS = ", JPASS, ", JBOFF = ", JBOFF, " <========="

  IF (JBOFF == 1) THEN
    JBLK1L = JBLK1A
    JBLK1U = JBLK1B
  ELSEIF (JBOFF == 2) THEN
    JBLK1L = 1 
    JBLK1U = 7 
  ELSE
    CALL FIELD_ABORT ('UNEXPECTED JBOFF')
  ENDIF

  ALLOCATE (D1 (NPROMA1, 0:NFLEVG, JBLK1L:JBLK1U))
  
  PRINT *, " NPROMA1 = ", NPROMA1
  PRINT *, " LBOUND (D1) = ", LBOUND (D1)
  PRINT *, " UBOUND (D1) = ", UBOUND (D1)
  
  DO JBLK = JBLK1L, JBLK1U
    DO JLON = 1, NPROMA1
      D1 (JLON, :, JBLK) = FUNC (JLON, JBLK)
    ENDDO
  ENDDO
  
  DO JBLK = JBLK1L, JBLK1U
    WRITE (*, '(20I12)') D1 (:, 1, JBLK)
  ENDDO
  
  CALL FIELD_NEW (FD, DATA=D1, LBOUNDS=[1, 0, JBLK1L])
  
  ! Reshuffle on NPROMA2 arrays
  
  CALL FGS%INIT (KGPTOT=NPROMA1*NGPBLKS1, KLON_S=NPROMA1, KLON_G=NPROMA2, KBLKOFF=JBLK1A)
  
  IF (JPASS == 1) THEN
    Z2 => GATHER_HOST_DATA_RDWR (FGS, FD)
  ELSEIF (JPASS == 2) THEN
    Z2 => GATHER_DEVICE_DATA_RDWR (FGS, FD)
  ENDIF

  PRINT *, " NPROMA2 = ", NPROMA2
  PRINT *, " LBOUND (Z2) = ", LBOUND (Z2)
  PRINT *, " UBOUND (Z2) = ", UBOUND (Z2)

  IF (NPROMA2 /= SIZE (Z2, 1)) CALL FIELD_ABORT ('NPROMA MISMATCH')
  IF (LBOUND (Z2, 2) /= LBOUND (D1, 2)) CALL FIELD_ABORT ('DIMENSION MISMATCH')
  IF (UBOUND (Z2, 2) /= UBOUND (D1, 2)) CALL FIELD_ABORT ('DIMENSION MISMATCH')
  
  DO JBLK = 1, SIZE (Z2, 3)
    WRITE (*, '(20I12)') Z2 (:, 1, JBLK)
  ENDDO
  
  IF (JPASS == 1) THEN
    DO JBLK = 1, SIZE (Z2, 3)
      DO JLEV = 0, NFLEVG
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
        DO JLEV = 0, NFLEVG
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

  DO JBLK = JBLK1L, JBLK1U
    WRITE (*, '(20I12)') D1 (:, 1, JBLK)
  ENDDO
  
  DO JBLK = JBLK1L, JBLK1U
    DO JLEV = 0, NFLEVG
      DO JLON = 1, NPROMA1
        IF ((JBLK1A .LE. JBLK) .AND. (JBLK .LE. JBLK1B)) THEN
          IF (D1 (JLON, JLEV, JBLK) /= (JPASS + 1) * FUNC (JLON, JBLK)) THEN
            PRINT *, " JPASS = ", JPASS, " JLON = ", JLON, " JLEV = ", JLEV, &
          & " JBLK = ", JBLK, " D1 = ", D1 (JLON, JLEV, JBLK), &
          & (JPASS + 1) * FUNC (JLON, JBLK)
            CALL FIELD_ABORT ('VALUE ERROR')
          ENDIF
        ELSE
          IF (D1 (JLON, JLEV, JBLK) /= FUNC (JLON, JBLK)) THEN
            PRINT *, " JPASS = ", JPASS, " JLON = ", JLON, " JLEV = ", JLEV, &
          & " JBLK = ", JBLK, " D1 = ", D1 (JLON, JLEV, JBLK), &
          & FUNC (JLON, JBLK)
            CALL FIELD_ABORT ('VALUE ERROR')
          ENDIF
        ENDIF
      ENDDO
    ENDDO
  ENDDO
  
  DEALLOCATE (D1)
  

ENDDO
ENDDO

END PROGRAM
