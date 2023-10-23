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

PROGRAM GET_VIEW
        USE FIELD_MODULE
        USE FIELD_FACTORY_MODULE
        USE OMP_LIB
        USE PARKIND1

        CLASS(FIELD_2RB), POINTER :: W => NULL()
        REAL(KIND=JPRB), POINTER :: D(:,:)
        INTEGER :: NPROMA = 24
        INTEGER :: NBLOCKS= 100
        INTEGER :: IBLK,JLON
        REAL(KIND=JPRB), POINTER :: VIEW(:) => NULL()

        ALLOCATE(D(NPROMA, NBLOCKS))
        CALL FIELD_NEW(W, DATA=D)
        !$OMP PARALLEL PRIVATE(VIEW, JLON) 
        !$OMP DO
        DO IBLK=1,NBLOCKS
          VIEW => W%GET_VIEW(IBLK)
          DO JLON = 1, NPROMA
            VIEW(JLON) = 7
          ENDDO
        END DO
        !$OMP END DO
        !$OMP END PARALLEL
        IF (.NOT. ALL(D == 7)) THEN
                ERROR STOP
        END IF
        CALL FIELD_DELETE(W)
END PROGRAM GET_VIEW
