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

PROGRAM INIT_WRAPPER_GPU
        ! TEST IF DATA ARE CORRECTLY TRANSFERED ON GPU
        USE FIELD_MODULE
        USE FIELD_FACTORY_MODULE
        USE PARKIND1
        IMPLICIT NONE
        CLASS(FIELD_2RB), POINTER :: W => NULL()
        REAL(KIND=JPRB), ALLOCATABLE :: D(:,:)
        REAL(KIND=JPRB), POINTER :: D_GPU(:,:)
        LOGICAL :: RES
        INTEGER :: I,J

        ALLOCATE(D(10,10))
        D=7

        CALL FIELD_NEW(W, DATA=D)
        CALL W%GET_DEVICE_DATA_RDONLY(D_GPU)
        !$ACC SERIAL PRESENT(D_GPU) COPY(RES)
        RES=.TRUE.
        DO I=1,10
        DO J=1,10
        IF(D_GPU(I,J) /= 7) THEN
                RES = .FALSE.
        END IF
        END DO
        END DO
        !$ACC END SERIAL

        IF (.NOT. RES) THEN
                ERROR STOP
        END IF
        CALL FIELD_DELETE(W)
END PROGRAM INIT_WRAPPER_GPU
