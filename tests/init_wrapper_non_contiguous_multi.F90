! (C) Copyright 2022- ECMWF.
! (C) Copyright 2022- Meteo-France.
! (C) Copyright 2023- NVIDIA
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.

PROGRAM INIT_WRAPPER_NON_CONTIGUOUS_MULTI
  ! TEST IF WRAPPER WORKS WITH NON CONTIGUOUS ARRAY
  USE FIELD_MODULE
  USE PARKIND1
  USE FIELD_FACTORY_MODULE
  use iso_c_binding

  IMPLICIT NONE
  REAL(KIND=JPRB), ALLOCATABLE, TARGET :: D1(:,:,:,:,:), D2(:,:,:,:,:)
  CLASS(FIELD_2RB), POINTER :: W2 => NULL()
  REAL(KIND=JPRB), POINTER :: W2PTR(:,:)
  CLASS(FIELD_3RB), POINTER :: W3 => NULL()
  REAL(KIND=JPRB), POINTER :: W3PTR(:,:,:)
  CLASS(FIELD_4RB), POINTER :: W4 => NULL()
  REAL(KIND=JPRB), POINTER :: W4PTR(:,:,:,:)
  CLASS(FIELD_5RB), POINTER :: W5 => NULL()
  REAL(KIND=JPRB), POINTER :: W5PTR(:,:,:,:,:)
  integer(kind=8) :: ptr

  ALLOCATE(D1(7, 9, 11, 13, 15))
  ALLOCATE(D2(7, 9, 11, 13, 15))
  D1 = 0
  D2 = 0

  PRINT *, "begin 1 (should call FIELD_4RB_COPY_2D_DIM1_4_CONTIGUOUS)"
  CALL FIELD_NEW(W4, DATA=D1(1:1,:,:,:,3))
  CALL W4%GET_HOST_DATA_RDWR(W4PTR)
  W4PTR=42
  CALL W4%GET_DEVICE_DATA_RDWR(W4PTR)
#ifdef OMPGPU
  !$OMP TARGET MAP(TO:W4PTR)
#else
  !$ACC KERNELS DEFAULT(PRESENT)
#endif
  W4PTR(:,:,:,:)=92
#ifdef OMPGPU
  !$OMP END TARGET
#else
  !$ACC END KERNELS
#endif
  D1 = -1
  D2 = -1
  D2(1:1,:,:,:,3)=92
  CALL W4%GET_HOST_DATA_RDONLY(W4PTR)
  CALL FIELD_DELETE(W4)
  IF (ANY(D1/=D2)) ERROR STOP
  PRINT *, "end 1"
  PRINT *, ""

  PRINT *, "begin 2 (should call FIELD_4RB_COPY_2D_DIM1_3_CONTIGUOUS)"
  ! Should call DIM1_3
  CALL FIELD_NEW(W3, DATA=D1(:,2,:,:,3))
  CALL W3%GET_HOST_DATA_RDWR(W3PTR)
  W3PTR=51
  CALL W3%GET_DEVICE_DATA_RDWR(W3PTR)
#ifdef OMPGPU
  !$OMP TARGET MAP(TO:W3PTR)
#else
  !$ACC KERNELS DEFAULT(PRESENT)
#endif
  W3PTR(:,:,:)=61
#ifdef OMPGPU
  !$OMP END TARGET
#else
  !$ACC END KERNELS
#endif
  D1 = -1
  D2 = -1
  D2(:,2,:,:,3)=61
  CALL FIELD_DELETE(W3)
  IF (ANY(D1/=D2)) ERROR STOP
  PRINT *, "end 2"
  PRINT *, ""

  PRINT *, "begin 3 (should call FIELD_4RB_COPY_2D_DIM3_4_CONTIGUOUS)"
  CALL FIELD_NEW(W4, DATA=D1(:,:,4:8,:,3))
  CALL W4%GET_DEVICE_DATA_RDWR(W4PTR)
#ifdef OMPGPU
  !$OMP TARGET MAP(TO:W4PTR)
#else
  !$ACC KERNELS DEFAULT(PRESENT)
#endif
  W4PTR(:,:,:,:)=31
#ifdef OMPGPU
  !$OMP END TARGET
#else
  !$ACC END KERNELS
#endif
  D1 = -1
  D2 = -1
  D2(:,:,4:8,:,3)=31
  CALL FIELD_DELETE(W4)
  IF (ANY(D1/=D2)) ERROR STOP
  PRINT *, "end 3"
  PRINT *, ""

  PRINT *, "begin 4 (should call FIELD_3RB_COPY_2D_DIM1_2_CONTIGUOUS)"
  CALL FIELD_NEW(W3, DATA=D1(:,2,4:8,3:5,3))
  CALL W3%GET_DEVICE_DATA_RDWR(W3PTR)
#ifdef OMPGPU
  !$OMP TARGET MAP(TO:W3PTR)
#else
  !$ACC KERNELS DEFAULT(PRESENT)
#endif
  W3PTR(:,:,:)=91
#ifdef OMPGPU
  !$OMP END TARGET
#else
  !$ACC END KERNELS
#endif
  D1 = -1
  D2 = -1
  D2(:,2,4:8,3:5,3)=91
  CALL FIELD_DELETE(W3)
  IF (ANY(D1/=D2)) ERROR STOP
  PRINT *, "end 4"
  PRINT *, ""

  PRINT *, "begin 5 (should call FIELD_2RB_COPY_2D_DIM1_2_CONTIGUOUS)"
  CALL FIELD_NEW(W2, DATA=D1(:,2,4:8,8,3))
  CALL W2%GET_DEVICE_DATA_RDWR(W2PTR)
#ifdef OMPGPU
  !$OMP TARGET MAP(TO:W2PTR)
#else
  !$ACC KERNELS DEFAULT(PRESENT)
#endif
  W2PTR(:,:)=12.1
#ifdef OMPGPU
  !$OMP END TARGET
#else
  !$ACC END KERNELS
#endif
  D1 = -1
  D2 = -1
  D2(:,2,4:8,8,3)=12.1
  CALL FIELD_DELETE(W2)
  IF (ANY(D1/=D2)) ERROR STOP
  PRINT *, "end 5"
  PRINT *, ""

  PRINT *, "begin 6 (should call FIELD_4RB_COPY_2D_DIM2_4_CONTIGUOUS)"
  CALL FIELD_NEW(W4, DATA=D1(:,:,4,:,:))
  CALL W4%GET_DEVICE_DATA_RDWR(W4PTR)
#ifdef OMPGPU
  !$OMP TARGET MAP(TO:W4PTR)
#else
  !$ACC KERNELS DEFAULT(PRESENT)
#endif
  W4PTR(:,:,:,:)=22.1
#ifdef OMPGPU
  !$OMP END TARGET
#else
  !$ACC END KERNELS
#endif
  D1 = -1
  D2 = -1
  D2(:,:,4,:,:)=22.1
  CALL FIELD_DELETE(W4)
  IF (ANY(D1/=D2)) ERROR STOP
  PRINT *, "end 6"
  PRINT *, ""


  PRINT *, "begin 7 (should call FIELD_5RB_COPY_DIM5_CONTIGUOUS)"
  CALL FIELD_NEW(W5, DATA=D1(:,1:1,1:1,1:1,1:1))
  CALL W5%GET_DEVICE_DATA_RDWR(W5PTR)
#ifdef OMPGPU
  !$OMP TARGET MAP(TO:W5PTR)
#else
  !$ACC KERNELS DEFAULT(PRESENT)
#endif
  W5PTR(:,:,:,:,:)=1.1
#ifdef OMPGPU
  !$OMP END TARGET
#else
  !$ACC END KERNELS
#endif
  D1 = -1
  D2 = -1
  D2(:,1:1,1:1,1:1,1:1)=1.1
  CALL FIELD_DELETE(W5)
  IF (ANY(D1/=D2)) ERROR STOP
  PRINT *, "end 7"
  PRINT *, ""

  PRINT *, "begin 8 (should call FIELD_5RB_COPY_2D_DIM3_5_CONTIGUOUS)"
  CALL FIELD_NEW(W5, DATA=D1(:3,1:1,3:3,:,2:4))
  CALL W5%GET_DEVICE_DATA_RDWR(W5PTR)
#ifdef OMPGPU
  !$OMP TARGET MAP(TO:W5PTR)
#else
  !$ACC KERNELS DEFAULT(PRESENT)
#endif
  W5PTR(:,:,:,:,:)=1.2
#ifdef OMPGPU
  !$OMP END TARGET
#else
  !$ACC END KERNELS
#endif
  D1 = -1
  D2 = -1
  D2(:3,1:1,3:3,:,2:4)=1.2
  CALL FIELD_DELETE(W5)
  IF (ANY(D1/=D2)) ERROR STOP
  PRINT *, "end 8"
  PRINT *, ""

  PRINT *, "begin 9 (should call FIELD_5RB_COPY_2D_DIM3_5_CONTIGUOUS)"
  CALL FIELD_NEW(W5, DATA=D1(:,1:1,3:3,:,2:4))
  CALL W5%GET_DEVICE_DATA_RDWR(W5PTR)
#ifdef OMPGPU
  !$OMP TARGET MAP(TO:W5PTR)
#else
  !$ACC KERNELS DEFAULT(PRESENT)
#endif
  W5PTR(:,:,:,:,:)=2.5
#ifdef OMPGPU
  !$OMP END TARGET
#else
  !$ACC END KERNELS
#endif
  D1 = -1
  D2 = -1
  D2(:,1:1,3:3,:,2:4)=2.5
  CALL FIELD_DELETE(W5)
  IF (ANY(D1/=D2)) ERROR STOP
  PRINT *, "end 9"
  PRINT *, ""

  PRINT *, "begin 10 (should call FIELD_5RB_COPY_2D_DIM2_4_CONTIGUOUS)"
  CALL FIELD_NEW(W5, DATA=D1(:,1:1,:,1:5,2:4))
  CALL W5%GET_DEVICE_DATA_RDWR(W5PTR)
#ifdef OMPGPU
  !$OMP TARGET MAP(TO:W5PTR)
#else
  !$ACC KERNELS DEFAULT(PRESENT)
#endif
  W5PTR(:,:,:,:,:)=9.1
#ifdef OMPGPU
  !$OMP END TARGET
#else
  !$ACC END KERNELS
#endif
  D1 = -1
  D2 = -1
  D2(:,1:1,:,1:5,2:4)=9.1
  CALL FIELD_DELETE(W5)
  IF (ANY(D1/=D2)) ERROR STOP
  PRINT *, "end 10"
  PRINT *, ""

  PRINT *, "begin 11 (should call FIELD_5RB_COPY_2D_DIM2_4_CONTIGUOUS)"
  CALL FIELD_NEW(W5, DATA=D1(:,1:1,:,8:12,:))
  CALL W5%GET_DEVICE_DATA_RDWR(W5PTR)
#ifdef OMPGPU
  !$OMP TARGET MAP(TO:W5PTR)
#else
  !$ACC KERNELS DEFAULT(PRESENT)
#endif
  W5PTR(:,:,:,:,:)=8.1
#ifdef OMPGPU
  !$OMP END TARGET
#else
  !$ACC END KERNELS
#endif
  D1 = -1
  D2 = -1
  D2(:,1:1,:,8:12,:)=8.1
  CALL FIELD_DELETE(W5)
  IF (ANY(D1/=D2)) ERROR STOP
  PRINT *, "end 11"
  PRINT *, ""

  PRINT *, "begin 12 (should call FIELD_5RB_COPY_2D_DIM1_5_CONTIGUOUS)"
  CALL FIELD_NEW(W5, DATA=D1(3:7,:,:,:,3:3))
  CALL W5%GET_DEVICE_DATA_RDWR(W5PTR)
#ifdef OMPGPU
  !$OMP TARGET MAP(TO:W5PTR)
#else
  !$ACC KERNELS DEFAULT(PRESENT)
#endif
  W5PTR(:,:,:,:,:)=8.4
#ifdef OMPGPU
  !$OMP END TARGET
#else
  !$ACC END KERNELS
#endif
  D1 = -1
  D2 = -1
  D2(3:7,:,:,:,3:3)=8.4
  CALL FIELD_DELETE(W5)
  IF (ANY(D1/=D2)) ERROR STOP
  PRINT *, "end 12"
  PRINT *, ""

  PRINT *, "begin 13 (should call FIELD_5RB_COPY_2D_DIM1_5_CONTIGUOUS)"
  CALL FIELD_NEW(W5, DATA=D1(3:3,:,:,:,:))
  CALL W5%GET_DEVICE_DATA_RDWR(W5PTR)
#ifdef OMPGPU
  !$OMP TARGET MAP(TO:W5PTR)
#else
  !$ACC KERNELS DEFAULT(PRESENT)
#endif
  W5PTR(:,:,:,:,:)=12
#ifdef OMPGPU
  !$OMP END TARGET
#else
  !$ACC END KERNELS
#endif
  D1 = -1
  D2 = -1
  D2(3:3,:,:,:,:)=12
  CALL FIELD_DELETE(W5)
  IF (ANY(D1/=D2)) ERROR STOP
  PRINT *, "end 13"
  PRINT *, ""

  PRINT *, "begin 14 (should call FIELD_5RB_COPY_2D_DIM1_2_CONTIGUOUS)"
  CALL FIELD_NEW(W5, DATA=D1(1:4,1:9:2,:,3:12:3,:))
  CALL W5%GET_DEVICE_DATA_RDWR(W5PTR)
#ifdef OMPGPU
  !$OMP TARGET MAP(TO:W5PTR)
#else
  !$ACC KERNELS DEFAULT(PRESENT)
#endif
  W5PTR(:,:,:,:,:)=18
#ifdef OMPGPU
  !$OMP END TARGET
#else
  !$ACC END KERNELS
#endif
  D1 = -1
  D2 = -1
  D2(1:4,1:9:2,:,3:12:3,:)=18
  CALL FIELD_DELETE(W5)
  IF (ANY(D1/=D2)) ERROR STOP
  PRINT *, "end 14"
  PRINT *, ""

  PRINT *, "begin 15 (should call FIELD_5RB_COPY_2D_DIM1_2_CONTIGUOUS)"
  CALL FIELD_NEW(W5, DATA=D1(:,1:9:2,:,3:12:3,:))
  CALL W5%GET_DEVICE_DATA_RDWR(W5PTR)
#ifdef OMPGPU
  !$OMP TARGET MAP(TO:W5PTR)
#else
  !$ACC KERNELS DEFAULT(PRESENT)
#endif
  W5PTR(:,:,:,:,:)=19
#ifdef OMPGPU
  !$OMP END TARGET
#else
  !$ACC END KERNELS
#endif
  D1 = -1
  D2 = -1
  D2(:,1:9:2,:,3:12:3,:)=19
  CALL FIELD_DELETE(W5)
  IF (ANY(D1/=D2)) ERROR STOP
  PRINT *, "end 15"
  PRINT *, ""

  PRINT *, "begin 16 (should call FIELD_5RB_COPY_2D_DIM3_4_CONTIGUOUS)"
#ifdef _CUDA
  CALL FIELD_NEW(W5, DATA=D1(:,:,:,3:12:3,:), MAP_DEVPTR=.FALSE.)
#else
  CALL FIELD_NEW(W5, DATA=D1(:,:,:,3:12:3,:), MAP_DEVPTR=.TRUE.)
#endif
  CALL W5%GET_DEVICE_DATA_RDWR(W5PTR)

#ifdef OMPGPU
#ifdef _CUDA
  !$OMP TARGET IS_DEVICE_PTR(W5PTR)
#else
  !$OMP TARGET MAP(TO:W5PTR)
#endif
#else
#ifdef _CUDA
  !$ACC KERNELS DEVICEPTR(W5PTR)
#else
  !$ACC KERNELS PRESENT(W5PTR)
#endif
#endif
  W5PTR(:,:,:,:,:)=19
#ifdef OMPGPU
  !$OMP END TARGET
#else
  !$ACC END KERNELS
#endif
  D1 = -1
  D2 = -1
  D2(:,:,:,3:12:3,:)=19
  CALL FIELD_DELETE(W5)
  IF (ANY(D1/=D2)) ERROR STOP
  PRINT *, "end 16"
  PRINT *, ""

END PROGRAM INIT_WRAPPER_NON_CONTIGUOUS_MULTI
