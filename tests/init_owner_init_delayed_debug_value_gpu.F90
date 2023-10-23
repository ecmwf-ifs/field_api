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

PROGRAM INIT_OWNER_INIT_DELAYED_DEBUG_VALUE_GPU
        !TEST THAT ALL DATA ARE INITIALIZED TO THE DEBUG VALUE CHOOSED BY THE
        !USER WHEN COMBINED WITH DELAYED ALLOCATION ON GPU
        USE FIELD_MODULE
        USE FIELD_FACTORY_MODULE
        USE FIELD_INIT_DEBUG_VALUE_MODULE
        USE PARKIND1
        IMPLICIT NONE
        CLASS(FIELD_2IM), POINTER :: O => NULL()
        INTEGER(KIND=JPIM), POINTER :: PTR(:,:)
        LOGICAL :: OKAY
        integer :: i,j

        USE_INIT_DEBUG_VALUE = .TRUE.
        INIT_DEBUG_VALUE_JPIM=-123456789

        CALL FIELD_NEW(O, LBOUNDS=[1,1], UBOUNDS=[10,10], PERSISTENT=.TRUE., DELAYED=.TRUE.)
        CALL O%GET_DEVICE_DATA_RDONLY(PTR)

        OKAY=.TRUE.
        !$ACC PARALLEL LOOP PRESENT(PTR) COPY(OKAY)
        DO I=1,10
        DO J=1,10
        IF(PTR(I,J) /= -123456789)THEN
                OKAY=.FALSE.
        ENDIF
        END DO
        END DO
        !$ACC END PARALLEL

        IF(OKAY .EQV. .FALSE.)THEN
                ERROR STOP
        ENDIF

        CALL FIELD_DELETE(O)
END PROGRAM INIT_OWNER_INIT_DELAYED_DEBUG_VALUE_GPU
