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
# field_api_compile_options
# =========================
# 
# Define the compiler flags used to build FIELD_API. ::
#
#        field_api_compile_options()
#
##############################################################################

macro( field_api_compile_options )

  if(CMAKE_Fortran_COMPILER_ID MATCHES PGI|NVIDIA|NVHPC)
    ecbuild_add_fortran_flags("-Mlarge_arrays")
    ecbuild_add_fortran_flags("-gopt")
  
    ecbuild_add_fortran_flags("-Minfo=accel,all,ccff" BUILD DEBUG)
  
  # These are architecture/compiler/offload-library specific options 
  # that should really be coming from external input
  # set(CMAKE_Fortran_FLAGS "-gpu=cc70")
  endif ()
  
  if(CMAKE_Fortran_COMPILER_ID MATCHES Intel)
    ecbuild_add_fortran_flags("-check nocontiguous" BUILD DEBUG)
  endif()

  unset( FIELD_API_COMPILE_DEFINITIONS )
  # We define a custom preprocessor macro using execute_process to allow the use of compiler wrappers
  # which may not necessarily define the same macros as the underlying compiler
  execute_process(COMMAND ${CMAKE_Fortran_COMPILER} --version OUTPUT_VARIABLE FC_version)
  if (FC_version MATCHES "flang")
    list(APPEND FIELD_API_COMPILE_DEFINITIONS IS_LLVM_FLANG)
  endif()

endmacro()
