! (C) Copyright 2022- ECMWF.
! (C) Copyright 2022- Meteo-France.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.

PROGRAM CHECK_GPU_NUM
        ! CHECK NUMBER OF GPUs

#ifdef _OPENACC
        USE OPENACC
#endif
#ifdef OMPGPU
        USE OMP_LIB
#endif
        IMPLICIT NONE
        INTEGER :: DEV_TYPE, NUM_GPUS = 0

#ifdef _OPENACC
        DEV_TYPE = ACC_GET_DEVICE_TYPE()
        NUM_GPUS = ACC_GET_NUM_DEVICES(DEV_TYPE)
#endif
#ifdef OMPGPU
        NUM_GPUS = OMP_GET_NUM_DEVICES()
#endif

        IF(NUM_GPUS == 0)THEN
           ERROR STOP
        ENDIF

END PROGRAM CHECK_GPU_NUM
