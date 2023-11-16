! (C) Copyright 2022- ECMWF.
! (C) Copyright 2022- Meteo-France.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.

MODULE CONTAINER_MOD
  USE FIELD_MODULE, ONLY: FIELD_2RB, FIELD_3RB
  USE FIELD_FACTORY_MODULE, ONLY: FIELD_NEW
  USE PARKIND1, ONLY: JPRB
  IMPLICIT NONE

  TYPE FIELD_GROUP
    ! A group of associated fields and associated view pointers
    REAL(KIND=JPRB), POINTER :: A(:), B(:), C(:)
    CLASS(FIELD_2RB), POINTER :: F_A, F_B, F_C

    ! A collective owner field for shared allocations
    CLASS(FIELD_3RB), POINTER :: F_GROUP(:,:,:)
  END TYPE FIELD_GROUP

  TYPE VARIABLE_2RB
    ! A simple prognostic variable with two timestepping fields
    CLASS(FIELD_2RB), POINTER :: F_T0=>NULL(), F_T1=>NULL()
  END TYPE VARIABLE_2RB

  TYPE STATE
    ! A collection of VARIABLE objects for named field access

  END TYPE STATE

  
END MODULE CONTAINER_MOD

PROGRAM FIELD_OPENMP_FIRSTPRIVATE
  ! Test if FIRSTPRIVATE behaviour of FIELD object, as well as
  ! compound derived types that use FIELD objects, behaves as
  ! intended.

  USE PARKIND1, ONLY: JPRB
  USE OML_MOD, ONLY: OML_MAX_THREADS, OML_MY_THREAD, OML_SET_NUM_THREADS, OML_GET_NUM_THREADS

  USE FIELD_MODULE, ONLY: FIELD_2RB
  USE FIELD_FACTORY_MODULE, ONLY: FIELD_NEW, FIELD_DELETE

  USE CONTAINER_MOD, ONLY: VARIABLE_2RB
  IMPLICIT NONE

  CLASS(FIELD_2RB), POINTER :: F_GLOBAL => NULL()
  CLASS(FIELD_2RB), POINTER :: F_LOCAL => NULL()
  TYPE(VARIABLE_2RB) :: U
  REAL(KIND=JPRB), POINTER :: P_VIEW_G(:), P_VIEW_L(:)
  LOGICAL :: RES
  INTEGER :: I,J, TID, NUMT, IERR

  IERR = OML_SET_NUM_THREADS(3)

  CALL FIELD_NEW(F_GLOBAL, LBOUNDS=[10,1], UBOUNDS=[21,11], PERSISTENT=.TRUE.)
  CALL FIELD_NEW(F_LOCAL, LBOUNDS=[10,1], UBOUNDS=[21,11], PERSISTENT=.FALSE.)

  CALL FIELD_NEW(U%F_T0, LBOUNDS=[10,1], UBOUNDS=[21,11], PERSISTENT=.TRUE.)

  NUMT = OML_MAX_THREADS()

  print *, "ml805 maximum threads:", NUMT

  ! This one is tricky: Fails on Intel; runs, but post-fails on NVHPC
  
  ! !$OMP PARALLEL DO FIRSTPRIVATE(F_GLOBAL)
  ! DO I=1, 11
  !   P_VIEW_G => F_GLOBAL%GET_VIEW(I)
  !   DO J=1,21
  !     TID = OML_MY_THREAD()
  !     ! P_VIEW_G(J) = REAL(TID, KIND=JPRB)
  !     P_VIEW_G(J) = 66.6
  !   END DO
  ! END DO
  ! !$OMP END PARALLEL DO

  !$OMP PARALLEL DO FIRSTPRIVATE(U)
  DO I=1, 11
    P_VIEW_G => U%F_T0%GET_VIEW(I)
    DO J=1,21
      TID = OML_MY_THREAD()
      P_VIEW_G(J) = REAL(TID, KIND=JPRB)
      ! P_VIEW_G(J) = 66.6
    END DO
  END DO
  !$OMP END PARALLEL DO

  DO I=1, 11
    DO J=1,21
      print *, "ml805 U%F_T0%PTR(J, I)", U%F_T0%PTR(J, I)
    END DO
  END DO

  CALL FIELD_DELETE(F_GLOBAL)
  
END PROGRAM FIELD_OPENMP_FIRSTPRIVATE
