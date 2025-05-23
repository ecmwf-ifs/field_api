! (C) Copyright 2022- ECMWF.
! (C) Copyright 2022- Meteo-France.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.

PROGRAM GET_VIEW_GET_DEVICE_DATA
        !CHECK THAT DATA MODIFIED WITH A POINTER RETRIEVED FROM GET_VIEW ARE
        !TRANSFERED TO THE GPU

        USE FIELD_MODULE
        USE FIELD_FACTORY_MODULE
        USE PARKIND1
        USE OML_MOD
        USE FIELD_ABORT_MODULE
        IMPLICIT NONE

        CLASS(FIELD_2IM), POINTER :: O => NULL()
        INTEGER, PARAMETER :: NPROMA = 24
        INTEGER :: IBLK,JLON
        INTEGER(KIND=JPIM), POINTER :: VIEW(:) => NULL()
        INTEGER(KIND=JPIM), POINTER :: PTR(:,:) => NULL()
        LOGICAL :: OKAY

        CALL FIELD_NEW(O, LBOUNDS=[1,1], UBOUNDS=[NPROMA,1])
        !$OMP PARALLEL PRIVATE(VIEW, JLON) 
        !$OMP DO
        DO IBLK=1,OML_MAX_THREADS()
          VIEW => O%GET_VIEW(IBLK)
          DO JLON = 1, NPROMA
            VIEW(JLON) = 7
          ENDDO
        END DO
        !$OMP END DO
        !$OMP END PARALLEL

        CALL O%GET_DEVICE_DATA_RDWR(PTR)
        OKAY=.TRUE.
#ifdef OMPGPU
        !$OMP TARGET MAP(TO:PTR) MAP(TOFROM:OKAY)
#else
        !$ACC SERIAL PRESENT(PTR) COPY(OKAY)
#endif
        IF(.NOT. ALL(PTR == 7))THEN
                OKAY=.FALSE.
        ENDIF
#ifdef OMPGPU
        !$OMP END TARGET
#else
        !$ACC END SERIAL
#endif

        IF(OKAY .EQV. .FALSE.)THEN
                CALL FIELD_ABORT ("ERROR")
        ENDIF

        CALL FIELD_DELETE(O)
END PROGRAM GET_VIEW_GET_DEVICE_DATA
