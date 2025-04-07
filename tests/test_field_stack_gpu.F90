! (C) Copyright 2022- ECMWF.
! (C) Copyright 2022- Meteo-France.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.

PROGRAM TEST_FIELD_STACK_GPU
    USE FIELD_FACTORY_MODULE
    USE FIELD_MODULE
    USE FIELD_ABORT_MODULE
    USE FIELD_BASIC_MODULE
    USE PARKIND1

    IMPLICIT NONE

    INTEGER(KIND=JPIM) :: NLEV = 37
    INTEGER(KIND=JPIM) :: NPROMA = 24
    INTEGER(KIND=JPIM) :: NBLKS = 64

    REAL(KIND=JPRB), ALLOCATABLE :: BUFFER(:,:)

    CLASS(FIELD_2RB), POINTER :: F_BUFFER => NULL()
    CLASS(FIELD_3RB), POINTER :: F_BLOCKED_BUFFER => NULL()

    CLASS(FIELD_1RB), POINTER :: F_1DF => NULL()
    CLASS(FIELD_2RB), POINTER :: F_2DF => NULL(), F_3DF => NULL(), F_2DB => NULL()
    CLASS(FIELD_3RB), POINTER :: F_3DB => NULL(), F_4DB =>  NULL()

    TYPE(FIELD_BASIC_PTR), ALLOCATABLE :: BUFFER_MEMBERS(:)
    TYPE(FIELD_BASIC_PTR), ALLOCATABLE :: BLOCKED_BUFFER_MEMBERS(:)

    INTEGER(KIND=JPIM) :: NTOTAL, I, J, K
    INTEGER(KIND=JPIM) :: MEMBER_LBOUNDS(3)
    INTEGER(KIND=JPIM) :: MEMBER_MAP(6)
    INTEGER(KIND=JPIM) :: UBOUNDS(3)

    REAL(KIND=JPRB), POINTER :: BLOCKED_BUFFER_PTR(:,:,:) => NULL()
    REAL(KIND=JPRB), POINTER :: DEV_PTR_1D(:) => NULL()
    REAL(KIND=JPRB), POINTER :: DEV_PTR_2D(:,:) => NULL()
    REAL(KIND=JPRB), POINTER :: DEV_PTR_3D(:,:,:) => NULL()

    NTOTAL = 1 + NLEV + 2*NLEV

    ALLOCATE(BUFFER(NPROMA*NBLKS, NTOTAL))
    BUFFER = 0._JPRB

    !... Define member properties
    MEMBER_LBOUNDS = 0

    MEMBER_MAP(1) = 1
    MEMBER_MAP(2) = 1
    MEMBER_MAP(5) = 2
    MEMBER_MAP(6) = NLEV + 1
    MEMBER_MAP(3) = NLEV + 2
    MEMBER_MAP(4) = 3*NLEV + 1

    !...Create field stacks
    UBOUNDS(1) = NPROMA
    UBOUNDS(2) = NTOTAL
    UBOUNDS(3) = NBLKS
    CALL FIELD_NEW(F_BUFFER, BUFFER_MEMBERS, DATA=BUFFER, MEMBER_LBOUNDS=MEMBER_LBOUNDS, MEMBER_MAP=MEMBER_MAP)
    CALL FIELD_NEW(F_BLOCKED_BUFFER, BLOCKED_BUFFER_MEMBERS, UBOUNDS=UBOUNDS, &
    &              MEMBER_LBOUNDS=MEMBER_LBOUNDS, MEMBER_MAP=MEMBER_MAP, PERSISTENT=.TRUE.)

    !...Initialize owned buffer
    CALL F_BLOCKED_BUFFER%GET_HOST_DATA_RDWR(BLOCKED_BUFFER_PTR)
    BLOCKED_BUFFER_PTR = 0._JPRB

    !...Associate named fields to stack members
    DO I=1,3
      ASSOCIATE(FLD_BASIC => BUFFER_MEMBERS(I)%PTR)
  
      IF(I == 1)THEN
        SELECT TYPE(FLD_BASIC)
        CLASS IS(FIELD_1RB)
           F_1DF => FLD_BASIC
        END SELECT
      ELSEIF(I == 3)THEN
        SELECT TYPE(FLD_BASIC)
        CLASS IS(FIELD_2RB)
           F_2DF => FLD_BASIC
        END SELECT
      ELSE
        SELECT TYPE(FLD_BASIC)
        CLASS IS(FIELD_2RB)
           F_3DF => FLD_BASIC
        END SELECT
      ENDIF

      END ASSOCIATE

      ASSOCIATE(FLD_BASIC => BLOCKED_BUFFER_MEMBERS(I)%PTR)
  
      IF(I == 1)THEN
        SELECT TYPE(FLD_BASIC)
        CLASS IS(FIELD_2RB)
           F_2DB => FLD_BASIC
        END SELECT
      ELSEIF(I == 3)THEN
        SELECT TYPE(FLD_BASIC)
        CLASS IS(FIELD_3RB)
           F_3DB => FLD_BASIC
        END SELECT
      ELSE
        SELECT TYPE(FLD_BASIC)
        CLASS IS(FIELD_3RB)
           F_4DB => FLD_BASIC
        END SELECT
      ENDIF

      END ASSOCIATE
    ENDDO

    !...Update named members of non-blocked buffer
    CALL F_1DF%GET_DEVICE_DATA_RDWR(DEV_PTR_1D)
#ifdef OMPGPU
    !$omp target map(to:DEV_PTR_1D)
#else
    !$acc serial present(DEV_PTR_1D)
#endif
    DEV_PTR_1D = 1._JPRB
#ifdef OMPGPU
    !$omp end target
#else
    !$acc end serial
#endif

    CALL F_2DF%GET_DEVICE_DATA_RDWR(DEV_PTR_2D)
#ifdef OMPGPU
    !$omp target map(to:DEV_PTR_2D)
#else
    !$acc serial present(DEV_PTR_2D)
#endif
    DO J=0,NLEV-1
      DO I=1,NPROMA*NBLKS
        DEV_PTR_2D(I,J) = 2._JPRB
      ENDDO
    ENDDO
#ifdef OMPGPU
    !$omp end target
#else
    !$acc end serial
#endif

    CALL F_3DF%GET_DEVICE_DATA_RDWR(DEV_PTR_2D)
#ifdef OMPGPU
    !$omp target map(to:DEV_PTR_2D)
#else
    !$acc serial present(DEV_PTR_2D)
#endif
    DO J=0,2*NLEV-1
      DO I=1,NPROMA*NBLKS
        DEV_PTR_2D(I,J) = 3._JPRB
      ENDDO
    ENDDO
#ifdef OMPGPU
    !$omp end target
#else
    !$acc end serial
#endif

    CALL F_BUFFER%SYNC_HOST_RDWR()

    IF(ANY(BUFFER(:,1) /= 1._JPRB)) CALL FIELD_ABORT('NON-BLOCKED BUFFER 1D MEMBER WRONG')
    IF(ANY(BUFFER(:,2:NLEV+1) /= 2._JPRB)) CALL FIELD_ABORT('NON-BLOCKED BUFFER 2D MEMBER WRONG')
    IF(ANY(BUFFER(:,NLEV+2:3*NLEV+1) /= 3._JPRB)) CALL FIELD_ABORT('NON-BLOCKED BUFFER 3D MEMBER WRONG')

    !...Update named members of blocked buffer
    CALL F_2DB%GET_DEVICE_DATA_RDWR(DEV_PTR_2D)
#ifdef OMPGPU
    !$omp target map(to:DEV_PTR_2D)
#else
    !$acc serial present(DEV_PTR_2D)
#endif
    DO J=1,NBLKS
      DO I=1,NPROMA
        DEV_PTR_2D(I,J) = 1._JPRB
      ENDDO
    ENDDO
#ifdef OMPGPU
    !$omp end target
#else
    !$acc end serial
#endif

    CALL F_3DB%GET_DEVICE_DATA_RDWR(DEV_PTR_3D)
#ifdef OMPGPU
    !$omp target map(to:DEV_PTR_3D)
#else
    !$acc serial present(DEV_PTR_3D)
#endif
    DO K=1,NBLKS
      DO J=0,NLEV-1
        DO I=1,NPROMA
          DEV_PTR_3D(I,J,K) = 2._JPRB
        ENDDO
      ENDDO
    ENDDO
#ifdef OMPGPU
    !$omp end target
#else
    !$acc end serial
#endif

    CALL F_4DB%GET_DEVICE_DATA_RDWR(DEV_PTR_3D)
#ifdef OMPGPU
    !$omp target map(to:DEV_PTR_3D)
#else
    !$acc serial present(DEV_PTR_3D)
#endif
    DO K=1,NBLKS
      DO J=0,2*NLEV-1
        DO I=1,NPROMA
          DEV_PTR_3D(I,J,K) = 3._JPRB
        ENDDO
      ENDDO
    ENDDO
#ifdef OMPGPU
    !$omp end target
#else
    !$acc end serial
#endif

    CALL F_BLOCKED_BUFFER%SYNC_HOST_RDWR()

    IF(ANY(BLOCKED_BUFFER_PTR(:,1,:) /= 1._JPRB)) CALL FIELD_ABORT('BLOCKED BUFFER 2D MEMBER WRONG')
    IF(ANY(BLOCKED_BUFFER_PTR(:,2:NLEV+1,:) /= 2._JPRB)) CALL FIELD_ABORT('BLOCKED BUFFER 3D MEMBER WRONG')
    IF(ANY(BLOCKED_BUFFER_PTR(:,NLEV+2:3*NLEV+1,:) /= 3._JPRB)) CALL FIELD_ABORT('BLOCKED BUFFER 4D MEMBER WRONG')

    CALL FIELD_DELETE(F_BUFFER)
    CALL FIELD_DELETE(F_BLOCKED_BUFFER)

    DEALLOCATE(BUFFER_MEMBERS)
    DEALLOCATE(BLOCKED_BUFFER_MEMBERS)

    NULLIFY(F_1DF)
    NULLIFY(F_2DF)
    NULLIFY(F_3DF)

    NULLIFY(F_2DB)
    NULLIFY(F_3DB)
    NULLIFY(F_4DB)

    DEALLOCATE(BUFFER)

END PROGRAM TEST_FIELD_STACK_GPU
