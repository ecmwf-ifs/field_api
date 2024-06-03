! (C) Copyright 2022- ECMWF.
! (C) Copyright 2022- Meteo-France.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.

MODULE FIELD_CONSTANTS_MODULE

USE PARKIND1, ONLY : JPIM

IMPLICIT NONE

INTEGER (KIND=JPIM), PARAMETER :: NDEVFRESH =      INT(B'00000001', KIND=JPIM)
INTEGER (KIND=JPIM), PARAMETER :: NHSTFRESH =      INT(B'00000010', KIND=JPIM)
INTEGER (KIND=JPIM), PARAMETER :: UNALLOCATED =    INT(B'00000100', KIND=JPIM)
INTEGER (KIND=JPIM), PARAMETER :: UNINITIALIZED =  INT(B'00000011', KIND=JPIM)
INTEGER (KIND=JPIM), PARAMETER :: NH2D = 1, ND2H = 2
INTEGER (KIND=JPIM), PARAMETER :: NRD = INT(B'00000001', KIND=JPIM)
INTEGER (KIND=JPIM), PARAMETER :: NWR = INT(B'00000010', KIND=JPIM)
INTEGER (KIND=JPIM), PARAMETER :: NF2L = 1, NL2F = 2

END MODULE 
