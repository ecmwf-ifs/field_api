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

PROGRAM INIT_OWNER_GPU
        ! TEST IF DATA ARE CORRECTLY TRANSFERED ON GPU

        USE FIELD_MODULE
        USE FIELD_FACTORY_MODULE
        USE PARKIND1
        IMPLICIT NONE
        CLASS(FIELD_2RB), POINTER :: O => NULL()
        REAL(KIND=JPRB), POINTER :: PTR_CPU(:,:)
        REAL(KIND=JPRB), POINTER :: PTR_GPU(:,:)
        LOGICAL :: OKAY
        INTEGER :: I,J

        CALL FIELD_NEW(O, LBOUNDS=[10,1], UBOUNDS=[21,11], PERSISTENT=.TRUE.)
        CALL O%GET_HOST_DATA_RDWR(PTR_CPU)
        PTR_CPU=42

        CALL O%GET_DEVICE_DATA_RDONLY(PTR_GPU)
        OKAY=.TRUE.
        !$ACC SERIAL PRESENT(PTR_GPU) COPY(OKAY)
        DO I=10,21
        DO J=1,11
        IF(PTR_GPU(I,J) /= 42) THEN
                OKAY = .FALSE.
        END IF
        END DO
        END DO
        !$ACC END SERIAL

        IF (.NOT. OKAY) THEN
                ERROR STOP
        END IF
        CALL FIELD_DELETE(O)
END PROGRAM INIT_OWNER_GPU
