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

PROGRAM RESIZE_OWNER
        !TEST IF RESIZING A OWNER IS WORKING

        USE FIELD_MODULE
        USE FIELD_FACTORY_MODULE
        USE PARKIND1
        IMPLICIT NONE
        CLASS(FIELD_2RB), POINTER :: O => NULL()
        REAL(KIND=JPRB), POINTER :: PTR(:,:)
        INTEGER(KIND=JPIM) :: L(2),U(2)

        CALL FIELD_NEW(O, LBOUNDS=[10,1], UBOUNDS=[21,11], PERSISTENT=.TRUE.)
        CALL O%GET_HOST_DATA_RDWR(PTR)
        PTR=42

        CALL FIELD_RESIZE(O, UBOUNDS=[100,100], PERSISTENT=.TRUE.)
        CALL O%GET_HOST_DATA_RDWR(PTR)
        PTR=7

        CALL O%GET_DIMS(LBOUNDS=L,UBOUNDS=U)
        IF(.NOT. U(1) == 100)THEN
                WRITE(*,*)"U(1) != 100"
                ERROR STOP
        END IF
        IF(.NOT. U(2) == 100)THEN
                WRITE(*,*)"U(2) != 100"
                ERROR STOP
        END IF
        IF (.NOT. ALL(PTR == 7)) THEN
                WRITE(*,*)"PTR != 7"
                ERROR STOP
        END IF 
        CALL FIELD_DELETE(O)
END PROGRAM RESIZE_OWNER
