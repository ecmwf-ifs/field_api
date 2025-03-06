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
# field_api_get_offload_model
# ===========================
# 
# Determine the GPU offload model to be used. ::
#
#        field_api_get_offload_model()
#
##############################################################################

macro( field_api_get_offload_model )

   ## check for OpenMP offload
   include(features/OMP)
   ecbuild_add_option( FEATURE OMP_OFFLOAD 
                       DEFAULT OFF
                       DESCRIPTION "Enable GPU offload via OpenMP"
                       CONDITION CMAKE_Fortran_COMPILER_ID MATCHES "PGI|NVHPC" AND ${_HAVE_OMP_OFFLOAD} )

   if( HAVE_OMP_OFFLOAD )
      set( FIELD_API_ENABLE_ACC OFF )
   endif()

   ## find OpenACC
   if( ${CMAKE_VERSION} VERSION_LESS "3.25" )
     if ( FIELD_API_ENABLE_ACC OR (NOT DEFINED FIELD_API_ENABLE_ACC AND (ENABLE_ACC OR NOT DEFINED ENABLE_ACC)) )
       # See https://gitlab.kitware.com/cmake/cmake/-/issues/23691, fixed in CMake 3.25
       # (TL;DR: FindOpenACC sets OpenACC_<LANG>_FOUND correctly but does not set
       #  OpenACC_FOUND unless all three C, CXX, and Fortran have been found - even if
       #  only one language has been requested via COMPONENTS)
       find_package( OpenACC COMPONENTS Fortran )
       if( OpenACC_Fortran_FOUND )
         set( OpenACC_FOUND ON )
       endif()
     endif()
   endif()
   ecbuild_add_option( FEATURE ACC
                       DEFAULT ON
                       DESCRIPTION "Support for using GPUs with OpenACC"
                       REQUIRED_PACKAGES "OpenACC COMPONENTS Fortran" 
                       CONDITION CMAKE_Fortran_COMPILER_ID MATCHES "PGI|NVHPC")

   ## check for CUDA
   include(CheckLanguage)
   check_language(CUDA)
   ecbuild_add_option( FEATURE CUDA
       DESCRIPTION "CUDA" DEFAULT ON
       CONDITION CMAKE_CUDA_COMPILER AND HAVE_ACC )

   set(FIELD_API_OFFLOAD_MODEL "HostOnly")
   if( HAVE_OMP_OFFLOAD )
     set(FIELD_API_OFFLOAD_MODEL "NVHPCOpenMP")
   else()
     if( HAVE_CUDA )
        set(FIELD_API_OFFLOAD_MODEL "NVHPCOpenACCCUDA")
     elseif( HAVE_ACC )
        set(FIELD_API_OFFLOAD_MODEL "NVHPCOpenACC")
     endif()
   endif()

   unset(FIELD_API_OFFLOAD_DEFINITIONS)
   if( HAVE_ACC OR HAVE_OMP_OFFLOAD )
      list(APPEND FIELD_API_OFFLOAD_DEFINITIONS WITH_GPU_OFFLOAD)
   endif()

endmacro()
