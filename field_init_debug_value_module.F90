!Copyright 2023 Meteo-France, ECMWF 
!
!Licensed under the Apache License, Version 2.0 (the "License");
!you may not use this file except in compliance with the License.
!You may obtain a copy of the License at
!
!    http://www.apache.org/licenses/LICENSE-2.0
!
!    Unless required by applicable law or agreed to in writing, software
!    distributed under the License is distributed on an "AS IS" BASIS,
!    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
!    See the License for the specific language governing permissions and
!    limitations under the License.

MODULE FIELD_INIT_DEBUG_VALUE_MODULE
  USE PARKIND1
  LOGICAL :: USE_INIT_DEBUG_VALUE = .FALSE.
  REAL(KIND=JPRM) :: INIT_DEBUG_VALUE_JPRM = 0.0_JPRM
  REAL(KIND=JPRB) :: INIT_DEBUG_VALUE_JPRB = 0.0_JPRB
  REAL(KIND=JPRD) :: INIT_DEBUG_VALUE_JPRD = 0.0_JPRD
  INTEGER(KIND=JPIM) :: INIT_DEBUG_VALUE_JPIM = 0_JPIM
  LOGICAL(KIND=JPLM) :: INIT_DEBUG_VALUE_JPLM = .FALSE.
END MODULE FIELD_INIT_DEBUG_VALUE_MODULE
