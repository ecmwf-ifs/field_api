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

PROGRAM INIT_OWNER_DELAYED_GPU
        !TEST IF DELAYED ALLOCATION IS WORKING ON GPU

        USE FIELD_MODULE
        USE FIELD_FACTORY_MODULE
        USE PARKIND1
        IMPLICIT NONE
        CLASS(FIELD_2RB), POINTER :: O => NULL()
        REAL(KIND=JPRB), POINTER :: PTR(:,:)
        LOGICAL :: OKAY
        INTEGER :: I,J

        CALL FIELD_NEW(O, LBOUNDS=[10,1], UBOUNDS=[21,11], DELAYED=.TRUE.)
        CALL O%GET_DEVICE_DATA_RDWR(PTR)
        !$ACC KERNELS PRESENT(PTR)
        PTR=42
        !$ACC END KERNELS

        OKAY=.TRUE.
        !$ACC SERIAL PRESENT(PTR) COPY(OKAY)
        IF(.NOT. ALL(PTR == 42))THEN
                OKAY=.FALSE.
        ENDIF
        !$ACC END SERIAL

        IF(OKAY .EQV. .FALSE.)THEN
                ERROR STOP
        ENDIF
        CALL FIELD_DELETE(O)
END PROGRAM INIT_OWNER_DELAYED_GPU
