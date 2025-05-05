! (C) Copyright 2022- ECMWF.
! (C) Copyright 2022- Meteo-France.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.

PROGRAM INIT_OWNER
        ! TEST IF OWNER IS REALLY ALLOCATING THE DATA
        ! WHEN PERSITENT IS SET TO FALSE OR IS NOT GIVEN IN ARGUMENT,
        ! THEN THE LAST DIM OF THE FIELD IS THE NUMBER OF OPENMP THREADS

        USE FIELD_MODULE
        USE FIELD_FACTORY_MODULE
        USE PARKIND1
        USE OMP_LIB, ONLY: OMP_GET_MAX_THREADS
        USE FIELD_ABORT_MODULE
        IMPLICIT NONE
        CLASS(FIELD_2RB), POINTER :: O => NULL()
        CLASS(FIELD_2RB), POINTER :: OFORCE => NULL()
        REAL(KIND=JPRB), POINTER  :: PTR(:,:), DEVPTR(:,:)
        INTEGER(KIND=JPIM)        :: I, J, OMP_MAX_THREADS

        CALL FIELD_NEW(O, LBOUNDS=[10,1], UBOUNDS=[21,11])
        CALL O%GET_HOST_DATA_RDWR(PTR)
        PTR=42

        IF (SIZE(O%PTR,1) /= 12) THEN
                CALL FIELD_ABORT ("ERROR")
        END IF
        IF (SIZE(O%PTR,2) /= OMP_GET_MAX_THREADS()) THEN
                CALL FIELD_ABORT ("ERROR")
        END IF

        IF (.NOT. ALL(O%PTR == 42)) THEN
                CALL FIELD_ABORT ("ERROR")
        END IF 
        CALL FIELD_DELETE(O)

        ! TEST THAT OWNER ALLOCATION ALSO WORKS WITH FORCE METHODS
        CALL FIELD_NEW(OFORCE, LBOUNDS=[10,1], UBOUNDS=[21,11])
        CALL OFORCE%GET_HOST_DATA_FORCE(PTR)
        PTR=42

        IF (SIZE(OFORCE%PTR,1) /= 12) THEN
                CALL FIELD_ABORT ("ERROR ")
        END IF

        IF (SIZE(OFORCE%PTR,2) /= OMP_GET_MAX_THREADS()) THEN
                CALL FIELD_ABORT ("ERROR")
        END IF

        IF (.NOT. ALL(OFORCE%PTR == 42)) THEN
                CALL FIELD_ABORT ("ERROR")
        END IF

        CALL FIELD_DELETE(OFORCE)

        CALL FIELD_NEW(OFORCE, LBOUNDS=[10,1], UBOUNDS=[21,11])
        CALL OFORCE%GET_DEVICE_DATA_FORCE(DEVPTR)
        OMP_MAX_THREADS = OMP_GET_MAX_THREADS()

#ifdef OMPGPU
        !$omp target map(to:DEVPTR)
#else
        !$acc serial present(DEVPTR)
#endif
        DO J=1,OMP_MAX_THREADS
          DO I=10,21
            DEVPTR(I,J)=45
          END DO
        END DO
#ifdef OMPGPU
        !$omp end target
#else
        !$acc end serial
#endif
        CALL OFORCE%GET_HOST_DATA_FORCE(PTR)

        IF (SIZE(OFORCE%PTR,1) /= 12) THEN
                CALL FIELD_ABORT ("ERROR ")
        END IF

        IF (SIZE(OFORCE%PTR,2) /= OMP_GET_MAX_THREADS()) THEN
                CALL FIELD_ABORT ("ERROR")
        END IF

        IF (.NOT. ALL(OFORCE%PTR == 45)) THEN
                CALL FIELD_ABORT ("ERROR")
        END IF

        CALL FIELD_DELETE(OFORCE)

END PROGRAM INIT_OWNER
