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

PROGRAM SYNC_HOST
        USE FIELD_MODULE
        USE FIELD_FACTORY_MODULE
        USE PARKIND1
        IMPLICIT NONE

        CLASS(FIELD_2RB), POINTER :: W => NULL()
        REAL(KIND=JPRB), ALLOCATABLE :: D(:,:)
        REAL(KIND=JPRB), POINTER :: D_GPU(:,:)
        INTEGER :: I, J

        ALLOCATE(D(10,10))
        D=3
        CALL FIELD_NEW(W, DATA=D)
        CALL W%GET_DEVICE_DATA_RDWR(D_GPU)
!$ACC KERNELS PRESENT(D_GPU)        
        DO I=1,10
        DO J=1,10
        D_GPU(I,J) = 7
        ENDDO
        ENDDO
!$ACC END KERNELS

        CALL W%SYNC_HOST_RDONLY(4)
        CALL WAIT_FOR_ASYNC_QUEUE(4)
        DO I=1,10
        DO J=1,10
        IF (D(I,J) /= 7) THEN
                STOP
        ENDIF
        ENDDO
        ENDDO
        CALL FIELD_DELETE(W)
END PROGRAM
