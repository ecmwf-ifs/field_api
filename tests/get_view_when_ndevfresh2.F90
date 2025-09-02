! (C) Copyright 2022- ECMWF.
! (C) Copyright 2022- Meteo-France.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.

PROGRAM GET_VIEW_WHEN_NDEVFRESH2
        !CHECK THAT IT'S POSSIBLE TO USE GET_VIEW WHEN THE DATA ARE FRESH ON DEVICE
        !IF THERE ARE MORE THAN 1 GPU, THEN DON'T USE THE FIRST ONE (NUMBERED 0)

        USE FIELD_MODULE
        USE FIELD_FACTORY_MODULE
        USE PARKIND1
        USE OML_MOD
#ifdef _OPENACC
        USE OPENACC
#endif
        IMPLICIT NONE

        CLASS(FIELD_2IM), POINTER :: O => NULL()
        INTEGER, PARAMETER :: NPROMA = 24
        INTEGER :: IBLK,JLON
        INTEGER(KIND=JPIM), POINTER :: VIEW(:) => NULL()
        INTEGER(KIND=JPIM), POINTER :: PTR(:,:) => NULL()

#ifdef _OPENACC
        IF(ACC_GET_NUM_DEVICES(ACC_GET_DEVICE_TYPE())>1)THEN
          CALL ACC_SET_DEVICE_NUM(1, ACC_GET_DEVICE_TYPE())
        ENDIF
#endif

        CALL FIELD_NEW(O, LBOUNDS=[1,1], UBOUNDS=[NPROMA,1])
        CALL O%GET_DEVICE_DATA_RDWR(PTR)
        PTR=7

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

        CALL FIELD_DELETE(O)
END PROGRAM GET_VIEW_WHEN_NDEVFRESH2
