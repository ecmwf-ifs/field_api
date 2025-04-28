! (C) Copyright 2022- ECMWF.
! (C) Copyright 2022- Meteo-France.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.

PROGRAM TEST_FIELD_ARRAY_WRAPPER
        ! TEST FIELD_ARRAY WRAPPER

        USE FIELD_ARRAY_MODULE
        USE PARKIND1
        USE FIELD_ABORT_MODULE
        IMPLICIT NONE
        TYPE(FIELD_2RB_ARRAY) :: W, WZERO
        REAL(KIND=JPRB), ALLOCATABLE :: D(:,:)

        IF(ASSOCIATED(W%F_P)) CALL FIELD_ABORT ("FIELD_ARRAY SHOULD REMAIN UNINITIALISED")

        ALLOCATE(D(10,10))
        D=7

        CALL W%INIT(D)
        D=42

        IF (.NOT. ALL(W%F_P%PTR == 42)) THEN
           CALL FIELD_ABORT ("ERROR")
        END IF 
        CALL W%FINAL()

        CALL W%INIT (UBOUNDS=[10,2])

        CALL W%FINAL ()

        IF(ASSOCIATED(W%F_P)) CALL FIELD_ABORT ("FIELD_ARRAY SHOULD HAVE BEEN NULLIFIED")
        DEALLOCATE(D)

        ! Test constructor with 0-sized allocatable
        ! =========================================
        ALLOCATE(D(10,0))
        CALL WZERO%INIT(D)
        IF (.NOT. ASSOCIATED(WZERO%F_P))  CALL FIELD_ABORT ("ERROR: Field not created")
        IF (.NOT. ASSOCIATED(WZERO%F_P%PTR))  CALL FIELD_ABORT ("ERROR: Field has no PTR")
        IF (SIZE(WZERO%F_P%PTR) /= 0)  CALL FIELD_ABORT ("ERROR: Field has wrong size")
        CALL WZERO%FINAL()
END PROGRAM TEST_FIELD_ARRAY_WRAPPER
