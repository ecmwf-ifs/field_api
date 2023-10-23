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

PROGRAM INIT_OWNER_DELAYED_INIT_DEBUG_VALUE
        !TEST THAT ALL DATA ARE INITIALIZE TO THE CHOOSEN VALUE
        USE FIELD_MODULE
        USE FIELD_FACTORY_MODULE
        USE FIELD_INIT_DEBUG_VALUE_MODULE
        USE PARKIND1
        USE OMP_LIB, ONLY: OMP_GET_MAX_THREADS
        IMPLICIT NONE
        CLASS(FIELD_2IM), POINTER :: O => NULL()
        INTEGER(KIND=JPIM), POINTER :: PTR(:,:)

        USE_INIT_DEBUG_VALUE = .TRUE.
        INIT_DEBUG_VALUE_JPIM=128128128

        CALL FIELD_NEW(O, LBOUNDS=[1,1], UBOUNDS=[10,10], DELAYED=.TRUE.)
        CALL O%GET_HOST_DATA_RDWR(PTR)
        IF (.NOT. ALL(PTR == 128128128)) THEN
                ERROR STOP
        END IF

        CALL FIELD_DELETE(O)
END PROGRAM INIT_OWNER_DELAYED_INIT_DEBUG_VALUE
