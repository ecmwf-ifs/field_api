! (C) Copyright 2022- ECMWF.
! (C) Copyright 2022- Meteo-France.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.

PROGRAM TEST_HDF5_OUTPUT
        USE FIELD_MODULE
        USE FIELD_FACTORY_MODULE
        USE PARKIND1
        USE FIELD_ABORT_MODULE
        USE HDF5
        IMPLICIT NONE

        CLASS(FIELD_2RB), POINTER :: W => NULL()
        REAL(KIND=JPRB), ALLOCATABLE :: D(:,:)
        REAL(KIND=JPRB), POINTER :: D_GPU(:,:)
        REAL(KIND=JPRB), POINTER :: D_CPU(:,:)
        TYPE(C_PTR) :: D_CPUPTR 
        INTEGER :: I, J

        ! HDF5 variables
        INTEGER(HID_T) :: file_id, dset_id, space_id
        INTEGER(HSIZE_T), DIMENSION(2) :: dims
        INTEGER :: hdferr

        ALLOCATE(D(10,10))
        D=3
        CALL FIELD_NEW(W, DATA=D)
        CALL W%GET_DEVICE_DATA_RDWR(D_GPU)
!$ACC KERNELS PRESENT(D_GPU)
        DO I=1,10
        DO J=1,10
        D_GPU(I,J) = 7
        ENDDO
        ENDDO
!$ACC END KERNELS

        CALL W%SYNC_HOST_RDONLY()
        DO I=1,10
        DO J=1,10
        IF (D(I,J) /= 7) THEN
                CALL FIELD_ABORT ("ERROR")
        ENDIF
        ENDDO
        ENDDO


        dims = SHAPE(D)  ! Get dimensions of the array
        CALL W%GET_HOST_DATA_RDWR(D_CPU)
        D_CPUPTR = C_LOC(D_CPU)
        CALL h5open_f(hdferr)
        CALL H5Screate_simple_f(2, dims, space_id, hdferr)
        CALL H5Fcreate_F("field_data.hdf5", H5F_ACC_TRUNC_F, file_id, hdferr)
        CALL H5Dcreate_f(file_id, "Field2D", H5T_NATIVE_DOUBLE, space_id, dset_id, hdferr)
        CALL H5Dwrite_f(dset_id, H5T_NATIVE_DOUBLE, D_CPUPTR, hdferr)

        CALL H5Dclose_f(dset_id, hdferr)
        CALL H5Sclose_F(space_id, hdferr)
        CALL H5Fclose_F(file_id, hdferr)

        CALL FIELD_DELETE(W)
END PROGRAM
