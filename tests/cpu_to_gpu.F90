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

PROGRAM CPU_TO_GPU
        !TEST INITIALISING DATA ON CPU THEN MOVING THEM ON GPU

        USE FIELD_MODULE
        USE FIELD_FACTORY_MODULE
        USE PARKIND1
        IMPLICIT NONE
        CLASS(FIELD_2IM), POINTER :: O => NULL()
        INTEGER(KIND=JPIM), POINTER :: PTR_HOST(:,:) => NULL()
        INTEGER(KIND=JPIM), POINTER :: PTR_DEV(:,:) => NULL()
        INTEGER ::I,J
        LOGICAL :: OKAY

        CALL FIELD_NEW(O, LBOUNDS=[1,1], UBOUNDS=[11,11], PERSISTENT=.TRUE.)
        CALL O%GET_HOST_DATA_RDWR(PTR_HOST)
        PTR_HOST=7

        IF (.NOT. ALL(PTR_HOST == 7)) THEN
                ERROR STOP
        END IF 

        CALL O%GET_DEVICE_DATA_RDONLY(PTR_DEV)

        OKAY=.TRUE.
        !$ACC PARALLEL PRESENT(PTR_DEV) COPY(OKAY)
        DO I=1,11
        DO J=1,11
          IF(PTR_DEV(I,J)/=7)THEN
                  OKAY=.FALSE.
          ENDIF
        ENDDO
        ENDDO
        !$ACC END PARALLEL
        IF(OKAY .EQV. .FALSE.)THEN
                ERROR STOP
        ENDIF
        CALL FIELD_DELETE(O)
END PROGRAM CPU_TO_GPU
