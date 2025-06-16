! (C) Copyright 2022- ECMWF.
! (C) Copyright 2022- Meteo-France.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.

PROGRAM FINAL_WRAPPER_GPU
        ! TEST IF DESTROYING THE DATA ON GPU WORKS

        USE FIELD_MODULE
        USE FIELD_FACTORY_MODULE
        USE PARKIND1
        USE FIELD_ABORT_MODULE
        IMPLICIT NONE
        CLASS(FIELD_2RB), POINTER :: W => NULL()
        REAL(KIND=JPRB), ALLOCATABLE :: D(:,:)
        REAL(KIND=JPRB), POINTER :: D_GPU(:,:)

        ALLOCATE(D(10,10))
        D=7

        CALL FIELD_NEW(W, DATA=D)
        CALL W%GET_DEVICE_DATA_RDONLY(D_GPU)
        write(*,*)"ICI",__LINE__

        IF (.NOT. ASSOCIATED(W%DEVPTR)) THEN
                CALL FIELD_ABORT ("ERROR")
        END IF

        write(*,*)"ICI",__LINE__
        CALL W%DELETE_DEVICE_DATA()
        write(*,*)"ICI",__LINE__

        IF (ASSOCIATED(W%DEVPTR)) THEN
                CALL FIELD_ABORT ("ERROR")
        END IF
        write(*,*)"ICI",__LINE__
        CALL FIELD_DELETE(W)
        write(*,*)"ICI",__LINE__
END PROGRAM FINAL_WRAPPER_GPU
