! (C) Copyright 2022- ECMWF.
! (C) Copyright 2022- Meteo-France.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.

PROGRAM NO_TRANSFER
        !TEST THAT DATA OF A FIELD OWNER IS NOT COPIED TO GPU WHEN THEY HAVE NOT
        !BEEN INITIALISED FIRST ON CPU

        USE FIELD_MODULE
        USE FIELD_FACTORY_MODULE
        USE PARKIND1
        USE FIELD_ABORT_MODULE
        IMPLICIT NONE
        CLASS(FIELD_2RB), POINTER :: O => NULL()
        REAL(KIND=JPRB), POINTER :: PTR(:,:)
        INTEGER ::I,J

        CALL FIELD_NEW(O, LBOUNDS=[1,1],UBOUNDS=[5,5])
        CALL O%GET_DEVICE_DATA_RDWR(PTR)

        WRITE(*,*)"O%STATS%TRANSFER_CPU_TO_GPU",O%STATS%TRANSFER_CPU_TO_GPU
        WRITE(*,*)"O%STATS%TRANSFER_GPU_TO_CPU",O%STATS%TRANSFER_GPU_TO_CPU
        IF(O%STATS%TRANSFER_CPU_TO_GPU ==1)THEN
                CALL FIELD_ABORT ("ERROR")
        ENDIF

#ifdef OMPGPU
        !$OMP TARGET TEAMS DISTRIBUTE PARALLEL DO MAP(TO:PTR)
#else
        !$ACC PARALLEL LOOP PRESENT(PTR)
#endif
        DO I=1,5
        DO J=1,5
        PTR(I,J)=7
        ENDDO
        ENDDO
#ifdef OMPGPU
        !$OMP END TARGET TEAMS DISTRIBUTE PARALLEL DO
#else
        !$ACC END PARALLEL LOOP
#endif

        CALL FIELD_DELETE(O)
END PROGRAM NO_TRANSFER
