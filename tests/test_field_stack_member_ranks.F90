! (C) Copyright 2022- ECMWF.
! (C) Copyright 2022- Meteo-France.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.

PROGRAM TEST_FIELD_STACK_MEMBER_RANKS
    USE FIELD_FACTORY_MODULE
    USE FIELD_MODULE
    USE FIELD_ABORT_MODULE
    USE PARKIND1

    IMPLICIT NONE

    INTEGER(KIND=JPIM) :: NLEV = 37
    INTEGER(KIND=JPIM) :: NPROMA = 24
    INTEGER(KIND=JPIM) :: NBLKS = 64

    CLASS(FIELD_3RB), POINTER :: F_BLOCKED_BUFFER => NULL()
    CLASS(FIELD_3RB), POINTER :: F_2DB => NULL(), F_3DB => NULL(), F_4DB =>  NULL()

    INTEGER(KIND=JPIM) :: NTOTAL, I, J, K
    INTEGER(KIND=JPIM) :: MEMBER_LBOUNDS(3)
    INTEGER(KIND=JPIM) :: MEMBER_MAP(6)
    INTEGER(KIND=JPIM) :: UBOUNDS(3)

    REAL(KIND=JPRB), POINTER :: BLOCKED_BUFFER_PTR(:,:,:) => NULL()
    REAL(KIND=JPRB), POINTER :: HOST_PTR_3D(:,:,:) => NULL()

    NTOTAL = 1 + NLEV + 2*NLEV

    !... Define member properties
    MEMBER_LBOUNDS = 0

    MEMBER_MAP(1) = 1
    MEMBER_MAP(2) = 1
    MEMBER_MAP(3) = NLEV + 2
    MEMBER_MAP(4) = 3*NLEV + 1
    MEMBER_MAP(5) = 2
    MEMBER_MAP(6) = NLEV + 1

    !...Create field stacks
    UBOUNDS(1) = NPROMA
    UBOUNDS(2) = NTOTAL
    UBOUNDS(3) = NBLKS
    CALL FIELD_NEW(F_BLOCKED_BUFFER, UBOUNDS=UBOUNDS, LSTACK=.TRUE., &
    &              MEMBER_LBOUNDS=MEMBER_LBOUNDS, MEMBER_MAP=MEMBER_MAP, MEMBER_RANKS=[3, 3, 3], &
    &              PERSISTENT=.TRUE.)

    !...Initialize owned buffer
    CALL F_BLOCKED_BUFFER%GET_HOST_DATA_RDWR(BLOCKED_BUFFER_PTR)
    BLOCKED_BUFFER_PTR = 0._JPRB

    !...Associate named fields to stack members
    CALL GET_STACK_MEMBER(F_BLOCKED_BUFFER, 1, F_2DB)
    CALL GET_STACK_MEMBER(F_BLOCKED_BUFFER, 2, F_4DB)
    CALL GET_STACK_MEMBER(F_BLOCKED_BUFFER, 3, F_3DB)

    !...Update named members of blocked buffer
    CALL F_2DB%GET_HOST_DATA_RDWR(HOST_PTR_3D)
    HOST_PTR_3D = 1._JPRB

    CALL F_3DB%GET_HOST_DATA_RDWR(HOST_PTR_3D)
    DO K=1,NBLKS
      DO J=0,NLEV-1
        DO I=1,NPROMA
          HOST_PTR_3D(I,J,K) = 2._JPRB
        ENDDO
      ENDDO
    ENDDO

    CALL F_4DB%GET_HOST_DATA_RDWR(HOST_PTR_3D)
    DO K=1,NBLKS
      DO J=0,2*NLEV-1
        DO I=1,NPROMA
          HOST_PTR_3D(I,J,K) = 3._JPRB
        ENDDO
      ENDDO
    ENDDO

    IF(ANY(BLOCKED_BUFFER_PTR(:,1,:) /= 1._JPRB)) CALL FIELD_ABORT('BLOCKED BUFFER 2D MEMBER WRONG')
    IF(ANY(BLOCKED_BUFFER_PTR(:,2:NLEV+1,:) /= 2._JPRB)) CALL FIELD_ABORT('BLOCKED BUFFER 3D MEMBER WRONG')
    IF(ANY(BLOCKED_BUFFER_PTR(:,NLEV+2:3*NLEV+1,:) /= 3._JPRB)) CALL FIELD_ABORT('BLOCKED BUFFER 4D MEMBER WRONG')

    CALL FIELD_DELETE(F_BLOCKED_BUFFER)

    NULLIFY(F_2DB)
    NULLIFY(F_3DB)
    NULLIFY(F_4DB)

END PROGRAM TEST_FIELD_STACK_MEMBER_RANKS
