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

PROGRAM NO_TRANSFER
        !TEST THAT DATA OF A FIELD OWNER IS NOT COPIED TO CPU WHEN THEY HAVE NOT
        !BEEN INITIALISED FIRST ON CPU OR GPU

        USE FIELD_MODULE
        USE FIELD_FACTORY_MODULE
        USE PARKIND1
        IMPLICIT NONE
        CLASS(FIELD_2RB), POINTER :: O => NULL()
        REAL(KIND=JPRB), POINTER :: PTR(:,:)
        INTEGER ::I,J

        CALL FIELD_NEW(O, LBOUNDS=[1,1],UBOUNDS=[5,5], PERSISTENT=.TRUE.)
        CALL O%GET_HOST_DATA_RDWR(PTR)

        WRITE(*,*)"O%STATS%TRANSFER_CPU_TO_GPU",O%STATS%TRANSFER_CPU_TO_GPU
        WRITE(*,*)"O%STATS%TRANSFER_GPU_TO_CPU",O%STATS%TRANSFER_GPU_TO_CPU
        IF(O%STATS%TRANSFER_GPU_TO_CPU /= 0)THEN
                ERROR STOP
        ENDIF

        DO I=1,5
        DO J=1,5
        PTR(I,J)=7
        ENDDO
        ENDDO

        !CALL FIELD_DELETE(O)
END PROGRAM NO_TRANSFER
