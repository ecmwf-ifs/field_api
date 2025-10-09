! (C) Copyright 2022- ECMWF.
! (C) Copyright 2022- Meteo-France.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.

PROGRAM TEST_HDF5_PARALLEL_OUTPUT
        USE PARKIND1, ONLY: JPRB, JPRM, JPRD, JPIM, JPLM
        USE FIELD_FACTORY_MODULE
        USE FIELD_MODULE, ONLY: FIELD_2RB, FIELD_2RM, FIELD_2RD, FIELD_2IM, FIELD_2LM
        USE FIELD_HDF5_MODULE, ONLY: WRITE_HDF5_DATA
        USE FIELD_ABORT_MODULE, ONLY: FIELD_ABORT
        USE HDF5, ONLY: H5open_F, H5close_F
        USE HDF5, ONLY: H5Fcreate_F, H5Fclose_F
        USE HDF5, ONLY: H5Screate_simple_F, H5Screate_simple_F, H5Sclose_F
        USE HDF5, ONLY: H5Lexists_F
        USE HDF5, ONLY: H5Dcreate_F, H5Dopen_F, H5Dclose_F, H5Dwrite_F, H5Dread_F
        USE HDF5, ONLY: HID_T, HSIZE_T
        USE HDF5, ONLY: H5_REAL_KIND, H5_INTEGER_KIND
        USE HDF5, ONLY: H5T_NATIVE_DOUBLE, H5T_NATIVE_INTEGER
        USE HDF5, ONLY: H5T_NATIVE_REAL, H5F_ACC_TRUNC_F
        USE MPL_MODULE, ONLY: MPL_INIT, MPL_END, MPL_RANK
        USE ISO_C_BINDING, ONLY : C_PTR, C_LOC
        IMPLICIT NONE

        CLASS(FIELD_2RB), POINTER :: WB => NULL()
        CLASS(FIELD_2RM), POINTER :: WM => NULL()
        CLASS(FIELD_2RD), POINTER :: WD => NULL()
        CLASS(FIELD_2IM), POINTER :: WI => NULL()
        CLASS(FIELD_2LM), POINTER :: WL => NULL()
        REAL(KIND=JPRB), ALLOCATABLE :: DB(:,:)
        REAL(KIND=JPRM), ALLOCATABLE :: DM(:,:)
        REAL(KIND=JPRD), ALLOCATABLE :: DD(:,:)
        INTEGER(KIND=JPIM), ALLOCATABLE :: DI(:,:)
        LOGICAL(KIND=JPLM), ALLOCATABLE :: DL(:,:)
        REAL(KIND=JPRB), POINTER :: DB_GPU(:,:)
        REAL(KIND=JPRM), POINTER :: DM_GPU(:,:)
        REAL(KIND=JPRD), POINTER :: DD_GPU(:,:)
        INTEGER(KIND=JPIM), POINTER :: DI_GPU(:,:)
        LOGICAL(KIND=JPLM), POINTER :: DL_GPU(:,:)
        REAL(KIND=JPRB), POINTER :: DB_CPU(:,:)
        REAL(KIND=JPRM), POINTER :: DM_CPU(:,:)
        REAL(KIND=JPRD), POINTER :: DD_CPU(:,:)
        INTEGER(KIND=JPIM), POINTER :: DI_CPU(:,:)
        LOGICAL(KIND=JPLM), POINTER :: DL_CPU(:,:)
        INTEGER :: I, J

        ! HDF5 variables
        INTEGER(HID_T) ::  file_id, space_id, kind_id
        INTEGER(HID_T) :: dsetb_id, dsetm_id, dsetd_id,dseti_id,dsetl_id
        INTEGER(HSIZE_T), DIMENSION(2) :: dims
        INTEGER :: hdferr
        INTEGER(JPIM) :: NPROCS
        CHARACTER(LEN=100) :: filenames

        CALL MPL_INIT(KPROCS=NPROCS,LDINFO=.FALSE.,LDENV=.TRUE.)
        WRITE(filenames, '(A,I0)') "field_store"
        filenames = TRIM(filenames) 

        ALLOCATE(DB(10,10))
        ALLOCATE(DM(10,10))
        ALLOCATE(DD(10,10))
        ALLOCATE(DI(10,10))
        ALLOCATE(DL(10,10))

        DB(:,:)=3._JPRB
        DM(:,:)=4._JPRM
        DD(:,:)=5._JPRD
        DI(:,:)=2
        DL(:,:)=.TRUE.

        CALL FIELD_NEW(WB, DATA=DB)
        CALL FIELD_NEW(WM, DATA=DM)
        CALL FIELD_NEW(WD, DATA=DD)
        CALL FIELD_NEW(WI, DATA=DI)
        CALL FIELD_NEW(WL, DATA=DL)
        CALL WB%GET_DEVICE_DATA_RDWR(DB_GPU)
        CALL WM%GET_DEVICE_DATA_RDWR(DM_GPU)
        CALL WD%GET_DEVICE_DATA_RDWR(DD_GPU)
        CALL WI%GET_DEVICE_DATA_RDWR(DI_GPU)
        CALL WL%GET_DEVICE_DATA_RDWR(DL_GPU)

#ifdef OMPGPU
!$OMP TARGET TEAMS DISTRIBUTE PARALLEL DO MAP(TO: DB_GPU,DM_GPU, DD_GPU, DI_GPU,  DL_GPU)
#else
!$ACC KERNELS PRESENT(DB_GPU,DM_GPU, DD_GPU, DI_GPU, DL_GPU)
#endif
        DO i=1,10
           DO j=1,10
              DB_GPU(I,J) = 7._JPRB
              DM_GPU(I,J) = 8._JPRM
              DD_GPU(I,J) = 9._JPRD
              DI_GPU(I,J) = 2
              DL_GPU(I,J) = .TRUE. 
           ENDDO
        ENDDO
#ifdef OMPGPU
!$OMP END TARGET TEAMS DISTRIBUTE PARALLEL DO
#else
!$ACC END KERNELS
#endif

        CALL WB%SYNC_HOST_RDONLY()
        CALL WM%SYNC_HOST_RDONLY()
        CALL WD%SYNC_HOST_RDONLY()
        CALL WI%SYNC_HOST_RDONLY()
        CALL WL%SYNC_HOST_RDONLY()

        DO i=1,10
           DO j=1,10
              IF (DB(I,J) /= 7._JPRB) THEN
                      CALL FIELD_ABORT ("JPRB variable not set or not synced properly.")
              ENDIF
              IF (DM(I,J) /= 8_JPRM) THEN
                      CALL FIELD_ABORT ("JPRM variable not set or not synced properly.")
              ENDIF
              IF (DD(I,J) /= 9._JPRD) THEN
                      CALL FIELD_ABORT ("JPRD variable not set or not synced properly.")
              ENDIF
              IF (DI(I,J) /= 2) THEN
                      CALL FIELD_ABORT ("Integer variable not set or not synced properly.")
              ENDIF
              IF (DL(I,J) .NEQV. .TRUE.) THEN
                      CALL FIELD_ABORT ("Logical variable not set or not synced properly.")
              ENDIF
           ENDDO
        ENDDO

        CALL WB%GET_HOST_DATA_RDWR(DB_CPU)
        CALL WM%GET_HOST_DATA_RDWR(DM_CPU)
        CALL WD%GET_HOST_DATA_RDWR(DD_CPU)
        CALL WI%GET_HOST_DATA_RDWR(DI_CPU)
        CALL WL%GET_HOST_DATA_RDWR(DL_CPU)

        DO CONCURRENT(i=1:10, j=1:10)
           DL_CPU(I,J) = .FALSE. 
        ENDDO

        CALL WRITE_HDF5_DATA(WB,DB_CPU,filenames,"WB")
        CALL WRITE_HDF5_DATA(WM,DM_CPU,filenames,"WM")
        CALL WRITE_HDF5_DATA(WD,DD_CPU,filenames,"WD")
        CALL WRITE_HDF5_DATA(WI,DI_CPU,filenames,"WI")
        CALL WRITE_HDF5_DATA(WL,DL_CPU,filenames,"WL")

        CALL MPL_END(LDMEMINFO=.FALSE.)

        CALL FIELD_DELETE(WB)
        CALL FIELD_DELETE(WM)
        CALL FIELD_DELETE(WD)
        CALL FIELD_DELETE(WI)
        CALL FIELD_DELETE(WL)
END PROGRAM
