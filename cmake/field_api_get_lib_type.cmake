# (C) Copyright 2026- ECMWF.
# (C) Copyright 2026- Meteo-France.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation
# nor does it submit to any jurisdiction.

##############################################################################
#.rst:
#
# field_api_get_lib_type
# ======================
#
# Determine whether to use shared or static linking::
#
#        field_api_get_lib_type()
#
##############################################################################

macro( field_api_get_lib_type )

  set( field_api_lib_type SHARED )
  if( DEFINED BUILD_SHARED_LIBS )
    if( ${BUILD_SHARED_LIBS} )
      set( field_api_lib_type SHARED )
    else()
      set( field_api_lib_type STATIC )
    endif()
  endif()
 
  if( DEFINED FIELD_API_BUILD_SHARED_LIBS )
    if( ${FIELD_API_BUILD_SHARED_LIBS} )
      set( field_api_lib_type SHARED )
    else()
      set( field_api_lib_type STATIC )
    endif()
  endif()

  set( field_api_offload_lib_type STATIC )
  if( CMAKE_Fortran_COMPILER_ID MATCHES "PGI|NVHPC" AND
      CMAKE_Fortran_COMPILER_VERSION VERSION_GREATER_EQUAL 25.7.0 )
      set( field_api_offload_lib_type SHARED )
  endif()

  if( (HAVE_ACC OR HAVE_OMP_OFFLOAD) )
    if( field_api_offload_lib_type MATCHES STATIC )
      ecbuild_warn("Enabling GPU offload forces static linking.")
      set( field_api_lib_type ${field_api_offload_lib_type} )
    endif()
  endif()

endmacro()
