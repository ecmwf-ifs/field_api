! (C) Copyright 2022- ECMWF.
! (C) Copyright 2022- Meteo-France.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.

PROGRAM TEST_FIELD_STACK_SIMPLE
    USE FIELD_FACTORY_MODULE
    USE FIELD_MODULE
    USE FIELD_ABORT_MODULE
    USE PARKIND1

    IMPLICIT NONE

    INTEGER(KIND=JPIM) :: NLEV = 37
    INTEGER(KIND=JPIM) :: NPROMA = 24
    INTEGER(KIND=JPIM) :: NBLKS = 64
    INTEGER(KIND=JPIM) :: NFLDS = 8

    CLASS(FIELD_4RB), POINTER :: F_STACK => NULL()
    CLASS(FIELD_3RB), POINTER :: F_MEMBER => NULL()

    INTEGER(KIND=JPIM) :: IFLD

    REAL(KIND=JPRB), POINTER :: STACK_PTR(:,:,:,:) => NULL()
    REAL(KIND=JPRB), POINTER :: MEMBER_PTR(:,:,:) => NULL()

    !...Create field stack
    CALL FIELD_NEW(F_STACK, UBOUNDS=[NPROMA, NLEV, NFLDS, NBLKS], LSTACK=.TRUE., PERSISTENT=.TRUE.)

    !...Initialize stack
    CALL F_STACK%GET_HOST_DATA_RDWR(STACK_PTR)
    STACK_PTR = 0._JPRB

    !...Write to stack members
    DO IFLD=1,NFLDS
       CALL GET_STACK_MEMBER(F_STACK, IFLD, F_MEMBER)
       CALL F_MEMBER%GET_HOST_DATA_RDWR(MEMBER_PTR)
       MEMBER_PTR(:,:,:) = REAL(IFLD, KIND=JPRB)
    ENDDO

    !...Check stack data
    DO IFLD=1,NFLDS
       IF(ANY(STACK_PTR(:,:,IFLD,:) /= REAL(IFLD, KIND=JPRB))) CALL FIELD_ABORT('STACK DATA INCORRECT.')
    ENDDO

    CALL FIELD_DELETE(F_STACK)
    NULLIFY(F_MEMBER)
    NULLIFY(STACK_PTR)
    NULLIFY(MEMBER_PTR)

END PROGRAM TEST_FIELD_STACK_SIMPLE
