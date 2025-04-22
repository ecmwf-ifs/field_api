! (C) Copyright 2022- ECMWF.
! (C) Copyright 2022- Meteo-France.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.

PROGRAM TEST_HDF5_PARALLEL_OUTPUT
        USE FIELD_MODULE
        USE FIELD_HDF5_MODULE
        USE FIELD_FACTORY_MODULE
        USE PARKIND1
        USE FIELD_ABORT_MODULE
        USE HDF5
        USE MPL_MODULE, ONLY: MPL_INIT, MPL_END, MPL_RANK
        USE MPL_DATA_MODULE
        IMPLICIT NONE

        CLASS(FIELD_2RB), POINTER :: W => NULL()
        CLASS(FIELD_2RM), POINTER :: WM => NULL()
        CLASS(FIELD_2RD), POINTER :: WD => NULL()
        CLASS(FIELD_2IM), POINTER :: WI => NULL()
        CLASS(FIELD_2LM), POINTER :: WL => NULL()
        REAL(KIND=JPRB), ALLOCATABLE :: D(:,:)
        REAL(KIND=JPRM), ALLOCATABLE :: DM(:,:)
        REAL(KIND=JPRD), ALLOCATABLE :: DD(:,:)
        INTEGER(KIND=JPIM), ALLOCATABLE :: DI(:,:)
        LOGICAL(KIND=JPLM), ALLOCATABLE :: DL(:,:)
        REAL(KIND=JPRB), POINTER :: D_GPU(:,:)
        REAL(KIND=JPRM), POINTER :: DM_GPU(:,:)
        REAL(KIND=JPRD), POINTER :: DD_GPU(:,:)
        INTEGER(KIND=JPIM), POINTER :: DI_GPU(:,:)
        LOGICAL(KIND=JPLM), POINTER :: DL_GPU(:,:)
        REAL(KIND=JPRB), POINTER :: D_CPU(:,:)
        REAL(KIND=JPRM), POINTER :: DM_CPU(:,:)
        REAL(KIND=JPRD), POINTER :: DD_CPU(:,:)
        INTEGER(KIND=JPIM), POINTER :: DI_CPU(:,:)
        LOGICAL(KIND=JPLM), POINTER :: DL_CPU(:,:)
        TYPE(C_PTR) :: D_CPUPTR 
        TYPE(C_PTR) :: DM_CPUPTR 
        TYPE(C_PTR) :: DD_CPUPTR 
        TYPE(C_PTR) :: DI_CPUPTR 
        TYPE(C_PTR) :: DL_CPUPTR 
        INTEGER :: I, J

        ! HDF5 variables
        INTEGER(HID_T) :: file_id, space_id, kind_id
        INTEGER(HID_T) :: dset_id, dsetm_id, dsetd_id,dseti_id,dsetl_id
        INTEGER(HSIZE_T), DIMENSION(2) :: dims
        INTEGER :: hdferr
        INTEGER(JPIM) :: NPROCS
        CHARACTER(LEN=100) :: filename,filenames
        CALL MPL_INIT(KPROCS=NPROCS,LDINFO=.FALSE.,LDENV=.TRUE.)
        WRITE(filename, '(A,I0)') "field_data_rank", MPL_RANK 
        WRITE(filenames, '(A,I0)') "field_store"
        filename = TRIM(filename) // ".hdf5"
        filenames = TRIM(filenames) 

        ALLOCATE(D(10,10))
        ALLOCATE(DM(10,10))
        ALLOCATE(DD(10,10))
        ALLOCATE(DI(10,10))
        ALLOCATE(DL(10,10))
        D=3
        DM=4
        DD=5
        DI=2
        DL=.TRUE.
        CALL FIELD_NEW(W, DATA=D)
        CALL FIELD_NEW(WM, DATA=DM)
        CALL FIELD_NEW(WD, DATA=DD)
        CALL FIELD_NEW(WI, DATA=DI)
        CALL FIELD_NEW(WL, DATA=DL)
        CALL W%GET_DEVICE_DATA_RDWR(D_GPU)
        CALL WM%GET_DEVICE_DATA_RDWR(DM_GPU)
        CALL WD%GET_DEVICE_DATA_RDWR(DD_GPU)
        CALL WI%GET_DEVICE_DATA_RDWR(DI_GPU)
        CALL WL%GET_DEVICE_DATA_RDWR(DL_GPU)
!$ACC KERNELS PRESENT(D_GPU,DM_GPU,DD_GPU)
        DO I=1,10
        DO J=1,10
        D_GPU(I,J) = 7
        DM_GPU(I,J) = 8
        DD_GPU(I,J) = 9
        DI_GPU(I,J) = 2
        DL_GPU(I,J) = .TRUE. 
        ENDDO
        ENDDO
!$ACC END KERNELS

        CALL W%SYNC_HOST_RDONLY()
        CALL WM%SYNC_HOST_RDONLY()
        CALL WD%SYNC_HOST_RDONLY()
        CALL WI%SYNC_HOST_RDONLY()
        CALL WL%SYNC_HOST_RDONLY()
        DO I=1,10
        DO J=1,10
        IF (D(I,J) /= 7) THEN
                CALL FIELD_ABORT ("ERROR")
        ENDIF
        IF (DM(I,J) /= 8) THEN
                CALL FIELD_ABORT ("ERROR")
        ENDIF
        IF (DD(I,J) /= 9) THEN
                CALL FIELD_ABORT ("ERROR")
        ENDIF
        IF (DI(I,J) /= 2) THEN
                CALL FIELD_ABORT ("ERROR")
        ENDIF
        IF (DL(I,J) .NEQV. .TRUE.) THEN
                CALL FIELD_ABORT ("ERROR")
        ENDIF
        ENDDO
        ENDDO

        dims = SHAPE(D)  ! Get dimensions of the array
        CALL W%GET_HOST_DATA_RDWR(D_CPU)
        CALL WM%GET_HOST_DATA_RDWR(DM_CPU)
        CALL WD%GET_HOST_DATA_RDWR(DD_CPU)
        CALL WI%GET_HOST_DATA_RDWR(DI_CPU)
        CALL WL%GET_HOST_DATA_RDWR(DL_CPU)
        D_CPUPTR  = C_LOC(D_CPU)
        DM_CPUPTR = C_LOC(DM_CPU)
        DD_CPUPTR = C_LOC(DD_CPU)
        DI_CPUPTR = C_LOC(DI_CPU)
        DL_CPUPTR = C_LOC(DL_CPU)
        CALL h5open_f(hdferr)
        CALL H5Screate_simple_f(2, dims, space_id, hdferr)
        CALL H5Fcreate_F(filename, H5F_ACC_TRUNC_F, file_id, hdferr)
        IF(JPRB == JPRD) THEN
!It is necessary the specialize the code, HDF5 constants must be passed directly
        CALL H5Dcreate_f(file_id, "W", H5T_NATIVE_DOUBLE, space_id, dset_id, hdferr)
        CALL H5Dwrite_f(dset_id, H5T_NATIVE_DOUBLE, D_CPUPTR, hdferr)
        ELSE 
        CALL H5Dcreate_f(file_id, "W", H5T_NATIVE_REAL, space_id, dsetm_id, hdferr)
        CALL H5Dwrite_f(dsetm_id, H5T_NATIVE_REAL, D_CPUPTR, hdferr)
        ENDIF
        CALL H5Dcreate_f(file_id, "WM", H5T_NATIVE_REAL, space_id, dsetm_id, hdferr)
        CALL H5Dcreate_f(file_id, "WD", H5T_NATIVE_DOUBLE, space_id, dsetd_id, hdferr)
        CALL H5Dcreate_f(file_id, "WI", H5T_NATIVE_INTEGER, space_id, dseti_id, hdferr)
        CALL H5Dcreate_f(file_id, "WL", H5T_NATIVE_INTEGER, space_id, dsetl_id, hdferr)
        CALL H5Dwrite_f(dsetm_id, H5T_NATIVE_REAL, DM_CPUPTR, hdferr)
        CALL H5Dwrite_f(dsetd_id, H5T_NATIVE_DOUBLE, DD_CPUPTR, hdferr)
        CALL H5Dwrite_f(dseti_id, H5T_NATIVE_INTEGER, DI_CPUPTR, hdferr)
        CALL H5Dwrite_f(dsetl_id, H5T_NATIVE_INTEGER, DL_CPUPTR, hdferr)

        CALL H5Dclose_f(dset_id, hdferr)
        CALL H5Dclose_f(dsetm_id, hdferr)
        CALL H5Dclose_f(dsetd_id, hdferr)
        CALL H5Dclose_f(dseti_id, hdferr)
        CALL H5Dclose_f(dsetl_id, hdferr)
        CALL H5Sclose_F(space_id, hdferr)
        CALL H5Fclose_F(file_id, hdferr)

        DO I=1,10
        DO J=1,10
        DL_CPU(I,J) = .FALSE. 
        ENDDO
        ENDDO
        CALL GET_STORAGE_DATA(W,D_CPU,filenames,"W")
        CALL GET_STORAGE_DATA(WM,DM_CPU,filenames,"WM")
        CALL GET_STORAGE_DATA(WD,DD_CPU,filenames,"WD")
        CALL GET_STORAGE_DATA(WI,DI_CPU,filenames,"WI")
        CALL GET_STORAGE_DATA(WL,DL_CPU,filenames,"WL")
        PRINT *,'MPL_WORLD_RANK',MPL_WORLD_RANK,MPL_RANK
        CALL MPL_END(LDMEMINFO=.FALSE.)
        CALL FIELD_DELETE(W)
        CALL FIELD_DELETE(WM)
        CALL FIELD_DELETE(WD)
        CALL FIELD_DELETE(WI)
        CALL FIELD_DELETE(WL)
END PROGRAM
