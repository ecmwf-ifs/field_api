# (C) Copyright 2022- ECMWF.
# (C) Copyright 2022- Meteo-France.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation
# nor does it submit to any jurisdiction.

macro( field_api_find_fiat_modules )

   ecbuild_find_package(NAME fiat COMPONENTS ${fiat_components})
   if( NOT fiat_FOUND )
     if(NOT UTIL_MODULE_PATH)
       ecbuild_critical("If not building with fiat, then the path for utility modules must be specified")
     endif()
   
     ecbuild_info( "Checking for FIAT components in ${UTIL_MODULE_PATH}" )
   
     find_file( ABOR1_PATH abor1.F90 REQUIRED
         HINTS ${UTIL_MODULE_PATH} ${UTIL_MODULE_PATH}/src/fiat/util
     )
     ecbuild_info( "Found ABOR1: ${ABOR1_PATH}" )
   
     find_file( OML_PATH oml_mod.F90 REQUIRED
         HINTS ${UTIL_MODULE_PATH} ${UTIL_MODULE_PATH}/src/fiat/oml
     )
     ecbuild_info( "Found OML: ${OML_PATH}" )
   
     find_file( PARKIND1_PATH parkind1.F90 REQUIRED
         HINTS ${UTIL_MODULE_PATH} ${UTIL_MODULE_PATH}/src/parkind
     )
     ecbuild_info( "Found PARKIND1: ${PARKIND1_PATH}" )
     list(APPEND srcs ${ABOR1_PATH} ${OML_PATH} ${PARKIND1_PATH})
   endif()

endmacro()