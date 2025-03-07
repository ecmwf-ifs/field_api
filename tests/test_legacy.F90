! (C) Copyright 2022- ECMWF.
! (C) Copyright 2022- Meteo-France.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.

PROGRAM TEST_LEGACY

USE FIELD_MODULE
USE FIELD_FACTORY_MODULE
USE FIELD_ACCESS_MODULE
USE FIELD_ABORT_MODULE
USE FIELD_CONSTANTS_MODULE
USE FIELD_UTIL_MODULE, ONLY : LEGACY, LEGACY_FIELD_3RB

USE PARKIND1

IMPLICIT NONE

CLASS(FIELD_3RB), POINTER :: FU, FV, FT

REAL(KIND=JPRB), ALLOCATABLE :: GMV (:,:,:,:)

REAL(KIND=JPRB), POINTER :: U (:,:,:), V (:,:,:), T (:,:,:)

INTEGER, PARAMETER :: NFLEVG = 5, NPROMA = 32, NGPBLKS = 4, NDIM = 3
INTEGER, PARAMETER :: IU = 1, IV = 2, IT = 3

INTEGER*8 :: IADDRL, IADDRU

INTEGER :: JLON, JLEV, JBLK
INTEGER :: IERR

ALLOCATE (GMV (NPROMA, NFLEVG, NDIM, NGPBLKS))

GMV = 0._JPRB

CALL FIELD_NEW (FU, DATA=GMV (:,:,IU,:))
CALL FIELD_NEW (FV, DATA=GMV (:,:,IV,:))
CALL FIELD_NEW (FT, DATA=GMV (:,:,IT,:))

! All fields on the device

U => GET_DEVICE_DATA_RDWR (FU)
V => GET_DEVICE_DATA_RDONLY (FV)
T => GET_DEVICE_DATA_RDONLY (FT)

#ifdef OMPGPU
!$omp target map(to:U)
#else
!$acc kernels present (U)
#endif
U(:,:,:) = 1._JPRB
#ifdef OMPGPU
!$omp end target
#else
!$acc end kernels
#endif

#ifdef OMPGPU
!$omp target enter data map(to:GMV)
#else
!$acc enter data create (GMV)
!$acc update device (GMV)
#endif

IADDRL = LOC (GMV (1,1,1,1))
IADDRU = LOC (GMV(NPROMA,NFLEVG,NDIM,NGPBLKS))

CALL LEGACY_FIELD_3RB (FU, KADDRL=IADDRL, KADDRU=IADDRU, KDIR=NF2L)

IERR = 0

#ifdef OMPGPU
!$omp target map(to:GMV) map(tofrom:IERR)
#else
!$acc serial present (GMV) copy (IERR)
#endif

DO JBLK = 1, NGPBLKS
  DO JLEV = 1, NFLEVG  
    DO JLON = 1, NPROMA
      IF (GMV (JLON,JLEV,IU,JBLK) /= 1._JPRB) THEN
        IERR = IERR + 1
      ENDIF
      GMV (JLON,JLEV,IV,JBLK) = 2._JPRB
    ENDDO
  ENDDO
ENDDO

#ifdef OMPGPU
!$omp end target
#else
!$acc end serial
#endif

IF (IERR /= 0) CALL FIELD_ABORT ('VALUE MISMATCH: KDIR=NF2L')

CALL LEGACY_FIELD_3RB (FV, KADDRL=IADDRL, KADDRU=IADDRU, KDIR=NL2F)

U => GET_DEVICE_DATA_RDONLY (FU)
V => GET_DEVICE_DATA_RDONLY (FV)
T => GET_DEVICE_DATA_RDONLY (FT)

IERR = 0

#ifdef OMPGPU
!$omp target map(to:U, V, T) map(tofrom:IERR)
#else
!$acc serial present (U, V, T) copy (IERR)
#endif

DO JBLK = 1, NGPBLKS
  DO JLEV = 1, NFLEVG  
    DO JLON = 1, NPROMA

        IF (U (JLON, JLEV, JBLK) /= 1._JPRB) THEN
          IERR = IERR + 1
        ENDIF
        
        IF (V (JLON, JLEV, JBLK) /= 2._JPRB) THEN
          IERR = IERR + 1
        ENDIF
        
        IF (T (JLON, JLEV, JBLK) /= 0._JPRB) THEN
          IERR = IERR + 1
        ENDIF

    ENDDO
  ENDDO
ENDDO

#ifdef OMPGPU
!$omp end target
#else
!$acc end serial
#endif

IF (IERR /= 0) CALL FIELD_ABORT ('VALUE MISMATCH: KDIR=NL2F')

#ifdef OMPGPU
!$omp target exit data map(delete:GMV)
#else
!$acc exit data delete (GMV)
#endif

CALL FIELD_DELETE (FT)
CALL FIELD_DELETE (FV)
CALL FIELD_DELETE (FU)

DEALLOCATE (GMV)

END PROGRAM 
