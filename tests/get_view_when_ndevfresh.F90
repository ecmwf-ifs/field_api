!Copyright 2023 Meteo-France, ECMWF 
!
!Licensed under the Apache License, Version 2.0 (the "License");
!you may not use this file except in compliance with the License.
!You may obtain a copy of the License at
!
!    http://www.apache.org/licenses/LICENSE-2.0
!
!    Unless required by applicable law or agreed to in writing, software
!    distributed under the License is distributed on an "AS IS" BASIS,
!    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
!    See the License for the specific language governing permissions and
!    limitations under the License.

PROGRAM GET_VIEW_WHEN_NDEVFRESH
        !CHECK THAT IT'S NOT POSSIBLE TO USE GET_VIEW WHEN THE DATA ARE 
        !FRESH ON DEVICE

        USE FIELD_MODULE
        USE FIELD_FACTORY_MODULE
        USE PARKIND1
        USE OML_MOD
        IMPLICIT NONE

        CLASS(FIELD_2IM), POINTER :: O => NULL()
        INTEGER, PARAMETER :: NPROMA = 24
        INTEGER :: IBLK,JLON
        INTEGER(KIND=JPIM), POINTER :: VIEW(:) => NULL()
        INTEGER(KIND=JPIM), POINTER :: PTR(:,:) => NULL()

        CALL FIELD_NEW(O, LBOUNDS=[1,1], UBOUNDS=[NPROMA,1])
        CALL O%GET_DEVICE_DATA_RDWR(PTR)
        PTR=7

        !$OMP PARALLEL PRIVATE(VIEW, JLON) 
        !$OMP DO
        DO IBLK=1,OML_MAX_THREADS()
        !GET VIEW SHOULD CRASH
          VIEW => O%GET_VIEW(IBLK)
          DO JLON = 1, NPROMA
            VIEW(JLON) = 7
          ENDDO
        END DO
        !$OMP END DO
        !$OMP END PARALLEL

        !THE PROGRAM SHOULD NEVER REACH HERE

        CALL FIELD_DELETE(O)
END PROGRAM GET_VIEW_WHEN_NDEVFRESH
