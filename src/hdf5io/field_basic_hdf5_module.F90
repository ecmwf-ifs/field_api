! (C) Copyright 2022- ECMWF.
! (C) Copyright 2022- Meteo-France.
! (C) Copyright 2023- NVIDIA
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.

MODULE FIELD_BASIC_HDF5_MODULE

USE OML_MOD, ONLY: OML_MAX_THREADS , OML_MY_THREAD 
USE HDF5

IMPLICIT NONE

CONTAINS

  SUBROUTINE OPEN_HDF5_STORAGE(FILENAME, FILE_ID)
    CHARACTER(LEN=*), INTENT(IN) :: FILENAME

    INTEGER(HID_T), INTENT(OUT) :: file_id
    INTEGER :: hdferr
    LOGICAL :: hdf5_file_exists, hdf5_dataset_exists 

        CALL H5open_F(hdferr)
        INQUIRE(FILE=TRIM(FILENAME), EXIST=hdf5_file_exists)
        IF (hdf5_file_exists) THEN
            CALL H5Fopen_F(TRIM(FILENAME), H5F_ACC_RDWR_F, file_id, hdferr)
        ELSE
            CALL H5Fcreate_F(TRIM(FILENAME), H5F_ACC_TRUNC_F, file_id, hdferr)
        END IF
  END SUBROUTINE OPEN_HDF5_STORAGE

  SUBROUTINE CLOSE_HDF5_STORAGE(DSET_ID, SPACE_ID, FILE_ID)
    INTEGER(HID_T) :: file_id, space_id
    INTEGER(HID_T) :: dset_id
    INTEGER :: hdferr
        CALL H5Dclose_f( dset_id, hdferr)
        CALL H5Sclose_F(space_id, hdferr)
        CALL H5Fclose_F( file_id, hdferr)
        CALL H5close_F( hdferr)
    END SUBROUTINE CLOSE_HDF5_STORAGE


END MODULE FIELD_BASIC_HDF5_MODULE
