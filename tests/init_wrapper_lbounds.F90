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

PROGRAM INIT_WRAPPER_LBOUNDS
        ! TEST IF WRAPPER IS USING THE ORIGINAL LBOUNDS
        USE FIELD_MODULE
        USE FIELD_FACTORY_MODULE
        USE PARKIND1
        IMPLICIT NONE
        CLASS(FIELD_2RB), POINTER :: W => NULL()
        REAL(KIND=JPRB), ALLOCATABLE :: D(:,:,:)
        INTEGER :: LBOUNDS(2)

        ALLOCATE(D(0:10, 5:10, 100:200))
        CALL FIELD_NEW(W, DATA=D(:,7,:), LBOUNDS=[0, 100])
        LBOUNDS = LBOUND(W%PTR)

        IF (LBOUNDS(1) /= 0 .OR. LBOUNDS(2) /= 100) THEN
                ERROR STOP
        END IF 
        CALL FIELD_DELETE(W)
END PROGRAM INIT_WRAPPER_LBOUNDS
