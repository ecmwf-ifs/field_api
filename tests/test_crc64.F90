! (C) Copyright 2022- ECMWF.
! (C) Copyright 2022- Meteo-France.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.

PROGRAM TEST_CRC64
        USE FIELD_MODULE
        USE FIELD_FACTORY_MODULE
        USE FIELD_UTIL_MODULE
        USE FIELD_ACCESS_MODULE
        USE OMP_LIB
        USE PARKIND1
        USE FIELD_ABORT_MODULE

        CLASS(FIELD_2RB), POINTER :: W => NULL()
        REAL(KIND=JPRB), POINTER :: D(:,:)
        INTEGER :: NPROMA = 24
        INTEGER :: NBLOCKS= 100
        INTEGER :: IBLK,JLON
        REAL(KIND=JPRB), POINTER :: VIEW(:) => NULL()

        INTEGER*8 :: ICRC64_PI, ICRC64_E, ICRC
#ifdef PARKIND1_SINGLE
        DATA ICRC64_PI / Z'CFD6EE8C3BC35928' /
        DATA ICRC64_E  / Z'FE1E016456AC91EC' /
#else
        DATA ICRC64_PI / Z'158D5E22445EA9D8' /
        DATA ICRC64_E  / Z'3A21FCC09C73ADA1' /
#endif

        ALLOCATE(D(NPROMA, NBLOCKS))

        CALL FIELD_NEW(W, DATA=D)

        D => GET_HOST_DATA_RDWR (W)

        D = 3.14

        ICRC = CRC64 (W)
!       WRITE (*, '(Z16.16)') ICRC
        IF (ICRC /= ICRC64_PI) CALL FIELD_ABORT ("ERROR")

        D => GET_DEVICE_DATA_RDWR (W)

!$acc kernels present (D)

        D = 2.78

!$acc end kernels

        ICRC = CRC64 (W)
!       WRITE (*, '(Z16.16)') ICRC
        IF (ICRC /= ICRC64_E) CALL FIELD_ABORT ("ERROR")
        

        CALL FIELD_DELETE(W)
END PROGRAM TEST_CRC64
