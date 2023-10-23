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

PROGRAM INIT_WRAPPER
        ! TEST IF WRAPPER IS REALLY WRAPPING DATA
        ! IF IT IS WRAPPING THEN MODIFYING THE WRAPED DATA SHOULD BE SEEN IN
        ! WRAPPER TOO

        USE FIELD_MODULE
        USE FIELD_FACTORY_MODULE
        USE PARKIND1
        IMPLICIT NONE
        CLASS(FIELD_2RB), POINTER :: W => NULL()
        REAL(KIND=JPRB), ALLOCATABLE :: D(:,:)

        ALLOCATE(D(10,10))
        D=7

        CALL FIELD_NEW(W, DATA=D)
        D=42

        IF (.NOT. ALL(W%PTR == 42)) THEN
                ERROR STOP
        END IF 
        CALL FIELD_DELETE(W)
END PROGRAM INIT_WRAPPER
