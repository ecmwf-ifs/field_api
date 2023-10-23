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

PROGRAM GATHER_SCATTER
        !TEST THAT FIELD_GATHSCAT ONLY MODIFY VALUES THAT HAVE BEEN FILTERED
        USE FIELD_MODULE
        USE FIELD_FACTORY_MODULE
        USE FIELD_ACCESS_MODULE
        USE FIELD_GATHSCAT_MODULE
        USE PARKIND1
        IMPLICIT NONE

        TYPE(FIELD_GATHSCAT):: FGS
        CLASS(FIELD_2LM), POINTER :: FTRIG => NULL()
        INTEGER, PARAMETER :: NPROMA = 10, NGPBLKS = 4
        LOGICAL :: TRIG(NPROMA,NGPBLKS)
        INTEGER ::I,J

        INTEGER(KIND=JPIM), ALLOCATABLE :: D(:,:)
        CLASS(FIELD_2IM),POINTER :: FD => NULL()
        INTEGER(KIND=JPIM),POINTER :: FILTERED_D(:,:) => NULL()


        !CREATE A FILTER TO USE WITH THE GATHSCAT
        TRIG=.FALSE.
        DO I=1,NPROMA
        IF(MOD(I,2)==0)THEN !ONLY MODIFY BLOCKS
                TRIG(I,:)=.TRUE.
        ENDIF
        ENDDO
        CALL FIELD_NEW(FTRIG, DATA=TRIG)

        !CREATE THE FIELD TO BE FILTERED BY GATHSCAT
        ALLOCATE(D(NPROMA,NGPBLKS))
        D=1
        CALL FIELD_NEW(FD, DATA=D)

        !FILTER DATA, WE GET A POINTER TO A CONTIGUOUS ARRAY CONTAINING ONLY THE FILTERED DATA
        CALL FGS%INIT(FTRIG, NPROMA*NGPBLKS)
        FILTERED_D=>GATHER_HOST_DATA_RDWR(FGS, FD)
        FILTERED_D=2 !NOT ALL OF D WILL BE MODIFIED, ONLY THE FILTERED DATA

        !ACTUALLY UPDATE THE D ARRAY WITH THE MODIFIED DATA
        CALL FGS%SCATTER()

        DO I=1,NPROMA
        IF(MOD(I,2)==0)THEN
                IF (.NOT. ALL(D(I,:)==2))THEN
                        ERROR STOP
                ENDIF
        ELSE
                IF (.NOT. ALL(D(I,:)==1))THEN
                        ERROR STOP
                ENDIF
        ENDIF
        ENDDO

        CALL FIELD_DELETE(FTRIG)

END PROGRAM
