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

PROGRAM FINAL_WRAPPER
        ! TEST IF DESTROYING THE WRAPPER DOESN'T DESTROY THE WRAPPED DATA

        USE FIELD_MODULE
        USE FIELD_FACTORY_MODULE
        USE PARKIND1
        IMPLICIT NONE
        CLASS(FIELD_2RB), POINTER :: W => NULL()
        REAL(KIND=JPRB), ALLOCATABLE :: D(:,:)

        ALLOCATE(D(10,10))
        D=7

        CALL FIELD_NEW(W, DATA=D)
        CALL W%FINAL()

        IF (ASSOCIATED(W%PTR)) THEN
                ERROR STOP
        END IF
        IF (ASSOCIATED(W%DEVPTR)) THEN
                ERROR STOP
        END IF

        IF (.NOT. ALLOCATED(D)) THEN
                ERROR STOP
        END IF

        IF (.NOT. ALL(D == 7)) THEN
                ERROR STOP
        END IF 
END PROGRAM FINAL_WRAPPER
