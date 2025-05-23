! (C) Copyright 2022- ECMWF.
! (C) Copyright 2022- Meteo-France.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.

PROGRAM MAIN

USE FIELD_MODULE
USE FIELD_FACTORY_MODULE
USE PARKIND1

IMPLICIT NONE

INTEGER (KIND=JPIM) :: NFLEVG, NPROMA, NGPBLKS
INTEGER (KIND=JPIM) :: JBLK,JLEV,JLON,I

REAL (KIND=JPRB), ALLOCATABLE :: ZDATA3 (:,:,:)
CLASS (FIELD_3RB), POINTER :: YLF3
CLASS (FIELD_4RB), POINTER :: YLF4

REAL (KIND=JPRB), POINTER  :: Z3 (:,:,:), Z4 (:,:,:,:)


NFLEVG =  9
NPROMA = 16
NGPBLKS = 4

ALLOCATE (ZDATA3 (NPROMA, NFLEVG, NGPBLKS))

DO JBLK = 1, NGPBLKS
  DO JLEV = 1, NFLEVG
    DO JLON = 1, NPROMA
      ZDATA3 (JLON, JLEV, JBLK) = JLON + NPROMA * (JBLK - 1) + JLEV * 100
    ENDDO
  ENDDO
ENDDO

CALL FIELD_NEW (YLF3, DATA=ZDATA3 (:,1:3,:), PERSISTENT=.TRUE.)

PRINT *, '-----------  HOST  ----------- R '
WRITE (*, '(B32.32)') YLF3%GET_STATUS ()
CALL YLF3%GET_HOST_DATA_RDONLY (Z3)
WRITE (*, '(B32.32)') YLF3%GET_STATUS ()

DO JLON = 1, NPROMA
  PRINT *, JLON, Z3 (JLON,1,1)
ENDDO

PRINT *, '----------- DEVICE ----------- R '
WRITE (*, '(B32.32)') YLF3%GET_STATUS ()
CALL YLF3%GET_DEVICE_DATA_RDONLY (Z3)
WRITE (*, '(B32.32)') YLF3%GET_STATUS ()

#ifdef OMPGPU
!$omp target map(to:Z3)
#else
!$acc serial present (Z3)
#endif
DO JLON = 1, NPROMA
  PRINT *, JLON, Z3 (JLON,1,1)
ENDDO
#ifdef OMPGPU
!$omp end target
#else
!$acc end serial
#endif

PRINT *, '----------- DEVICE ----------- W '
WRITE (*, '(B32.32)') YLF3%GET_STATUS ()
CALL YLF3%GET_DEVICE_DATA_RDWR (Z3)
WRITE (*, '(B32.32)') YLF3%GET_STATUS ()

#ifdef OMPGPU
!$omp target map(to:Z3)
#else
!$acc serial present (Z3)
#endif
DO JLON = 1, NPROMA
   Z3 (JLON,1,1) = REAL (JLON * JLON, 8)
ENDDO
#ifdef OMPGPU
!$omp end target
#else
!$acc end serial
#endif

PRINT *, '-----------  HOST  ----------- R '
WRITE (*, '(B32.32)') YLF3%GET_STATUS ()
CALL YLF3%GET_HOST_DATA_RDONLY (Z3)
WRITE (*, '(B32.32)') YLF3%GET_STATUS ()

DO JLON = 1, NPROMA
  PRINT *, JLON, Z3 (JLON,1,1)
ENDDO

CALL FIELD_DELETE (YLF3)

CALL FIELD_NEW (YLF4, LBOUNDS=[1, 0, 1, 1], UBOUNDS=[10, 15, 3, 5], PERSISTENT=.TRUE.)

CALL YLF4%GET_HOST_DATA_RDWR (Z4)
WRITE (*, *) " LBOUND (Z4) = ", LBOUND (Z4)
WRITE (*, *) " UBOUND (Z4) = ", UBOUND (Z4)

DO I = 1, 10
  Z4 (I,:,:,:) = 3.14 * REAL (I, 8)
ENDDO

CALL YLF4%GET_DEVICE_DATA_RDWR (Z4)

#ifdef OMPGPU
!$omp target map(to:Z4)
#else
!$acc serial present (Z4)
#endif
PRINT *, Z4 (1, 0, 1, 1)
PRINT *, Z4 (2, 1, 1, 1)
Z4 (:,2,:,:) = 0.
#ifdef OMPGPU
!$omp end target
#else
!$acc end serial
#endif

CALL YLF4%GET_HOST_DATA_RDONLY (Z4)

PRINT *, Z4 (1,:,1,1)

CALL FIELD_DELETE (YLF4)

END PROGRAM
