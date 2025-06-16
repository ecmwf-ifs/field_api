! (C) Copyright 2022- ECMWF.
! (C) Copyright 2022- Meteo-France.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.

PROGRAM GET_VIEW_WHEN_UNINITIALIZED
        !CHECK THAT IT IS POSSIBLE TO USE GET_VIEW WHEN THE DATA ARE
        !UNINITIALIZED

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

        CALL FIELD_NEW(O, LBOUNDS=[1,1], UBOUNDS=[NPROMA,1])
        IF(.NOT. ASSOCIATED(O%PTR)) THEN
          ! GET_VIEW IS SUPPOSED TO BE RUN ONLY IF THE DATA ARE ALLOCATED ON THE CPU
          ! IF IT'S NOT THE CASE THIS TEST-CASE IS CONSIDERED UNNECESSARY
          !
          ! SEE TEST CASE GET_VIEW_WHEN_UNALLOCATED FOR PROPER TESTING OF THIS CASE
          RETURN
        ENDIF

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

        CALL O%GET_HOST_DATA_RDONLY(PTR)
        IF (.NOT. ALL(PTR==7))THEN
                CALL FIELD_ABORT ("ERROR")
        ENDIF

        CALL FIELD_DELETE(O)
END PROGRAM GET_VIEW_WHEN_UNINITIALIZED
