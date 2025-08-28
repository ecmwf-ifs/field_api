! (C) Copyright 2022- ECMWF.
! (C) Copyright 2022- Meteo-France.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.

PROGRAM INIT_OWNER
        ! JUST TO CHECK AND PRINT IF THE DELAYED OPTION HA SBEEN ACTIVATED AT COMPILATION TIME
        USE FIELD_DEFAULTS_MODULE, ONLY: DELAYED_DEFAULT_VALUE
        USE FIELD_ABORT_MODULE

        WRITE(*,*)"IS DELAYED BY DEFAULT", DELAYED_DEFAULT_VALUE

        IF(DELAYED_DEFAULT_VALUE .EQV. .TRUE.) THEN
          ! THIS IS NOT A REAL ERROR
          CALL FIELD_ABORT ("DELAYED IS ACTIVATED BY DEFAULT")
        ENDIF

END PROGRAM INIT_OWNER

