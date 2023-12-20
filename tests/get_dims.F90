! (C) Copyright 2022- ECMWF.
! (C) Copyright 2022- Meteo-France.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.

PROGRAM GET_DIMS
        !TEST THAT GET DIMS RETURN THE RIGHT VALUES

        USE FIELD_MODULE
        USE FIELD_FACTORY_MODULE
        USE PARKIND1
        USE OML_MOD, ONLY: OML_MAX_THREADS
        IMPLICIT NONE
        CLASS(FIELD_2RB), POINTER :: O => NULL()
        CLASS(FIELD_2RB), POINTER :: W => NULL()
        REAL(KIND=JPRB) :: D(2:33,2:4)
        INTEGER :: L(2), U(2)

        CALL FIELD_NEW(O, LBOUNDS=[10,5], UBOUNDS=[21,11], PERSISTENT=.TRUE.)
        CALL O%GET_DIMS(LBOUNDS=L, UBOUNDS=U)
        IF(L(1)/= 10 .AND. L(2)/=5)THEN
                WRITE(*,*)"OWNER BAD LOWER BOUNDS"
                ERROR STOP
        END IF
        IF(U(1)/= 21 .AND. U(2) /= 11)THEN
                WRITE(*,*)"OWNER BAD UPPER BOUNDS"
                ERROR STOP
        END IF
        CALL FIELD_DELETE(O)


        CALL FIELD_NEW(O, LBOUNDS=[10,5], UBOUNDS=[21,11], PERSISTENT=.FALSE.)
        IF(L(1)/= 10 .AND. L(2)/=1)THEN
                WRITE(*,*)"OWNER BAD LOWER BOUNDS WHEN PERSISTENT"
                ERROR STOP
        END IF
        IF(U(1)/= 21 .AND. U(2) /= OML_MAX_THREADS())THEN
                WRITE(*,*)"OWNER BAD UPPER BOUNDS WHEN PERSISTENT"
                ERROR STOP
        END IF
        CALL FIELD_DELETE(O)


        CALL FIELD_NEW(W, DATA=D)
        CALL W%GET_DIMS(LBOUNDS=L, UBOUNDS=U)
        IF(L(1)/= 1 .AND. L(2)/=1)THEN
                WRITE(*,*)"WRAPPER BAD LOWER BOUNDS"
                ERROR STOP
        END IF
        IF(U(1)/= 32 .AND. U(2) /= 3)THEN
                WRITE(*,*)"WRAPPER BAD UPPER BOUNDS"
                ERROR STOP
        END IF
        CALL FIELD_DELETE(W)


        CALL FIELD_NEW(W, DATA=D, LBOUNDS=[2,2])
        CALL W%GET_DIMS(LBOUNDS=L, UBOUNDS=U)
        IF(L(1)/= 2 .AND. L(2)/=2)THEN
                WRITE(*,*)"WRAPPER BAD LOWER BOUNDS WITH LBOUNDS ARG"
                ERROR STOP
        END IF
        IF(U(1)/= 33 .AND. U(2) /= 4)THEN
                WRITE(*,*)"WRAPPER BAD UPPER BOUNDS WITH LBOUNDS ARG"
                ERROR STOP
        END IF
        CALL FIELD_DELETE(W)

END PROGRAM GET_DIMS

