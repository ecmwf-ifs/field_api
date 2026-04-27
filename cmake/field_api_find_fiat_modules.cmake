# (C) Copyright 2022- ECMWF.
# (C) Copyright 2022- Meteo-France.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation
# nor does it submit to any jurisdiction.

##############################################################################
#.rst:
#
# field_api_find_fiat_modules
# =========================
# 
# Find the FIAT utility modules needed to build FIELD_API. ::
#
#        field_api_find_fiat_modules()
#
##############################################################################

macro( field_api_find_fiat_modules )

   unset(fiat_srcs)
   if( NOT UTIL_MODULE_PATH )
     ecbuild_find_package(NAME fiat COMPONENTS ${fiat_components} )
     if (NOT fiat_FOUND)
       ecbuild_error("FIAT not detected. Specify fiat_ROOT or the path for utility modules, e.g. using -DUTIL_MODULE_PATH=")
     endif()
   else() 
     ecbuild_info( "UTIL_MODULE_PATH is provided. We will build independent of the full FIAT." )
     set(fiat_FOUND 0)
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
     list(APPEND fiat_srcs ${ABOR1_PATH} ${OML_PATH} ${PARKIND1_PATH})
   endif()

endmacro()
