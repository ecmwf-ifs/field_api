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

PROGRAM FINAL_OWNER
        ! TEST IF OWNER IS DESTROYED WHEN CALLING FINAL

        USE FIELD_MODULE
        USE FIELD_FACTORY_MODULE
        USE PARKIND1
        IMPLICIT NONE
        CLASS(FIELD_2RB), POINTER :: O => NULL()
        REAL(KIND=JPRB), POINTER :: PTR(:,:)

        CALL FIELD_NEW(O, [10,1],[21,11])
        CALL O%GET_HOST_DATA_RDWR(PTR)
        PTR=42
        CALL O%FINAL()

        IF (ASSOCIATED(O%PTR)) THEN
                ERROR STOP
        END IF
        IF (ASSOCIATED(O%DEVPTR)) THEN
                ERROR STOP
        END IF
        CALL FIELD_DELETE(O)
END PROGRAM FINAL_OWNER
