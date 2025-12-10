# (C) Copyright 2022- ECMWF.
# (C) Copyright 2022- Meteo-France.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation
# nor does it submit to any jurisdiction.

list(APPEND srcs
     field_basic_module.F90
     field_defaults_module.F90
     dev_alloc.c
     c_malloc.c
     field_constants_module.F90
     field_abort_module.F90
     mem_pool_module.F90)

list( APPEND ranksuff_srcs _data)
list( APPEND non_ranksuff_srcs dev_alloc_module field_module host_alloc_module field_async_module )

## expand ranksuff sources
field_api_expand_fypp_ranksuff(
   PYTHON_MODULE_DIR      ${PROJECT_SOURCE_DIR}/python_utils
   SOURCE_DIR             ${CMAKE_CURRENT_SOURCE_DIR}
   INPUT_SRCS             ${ranksuff_srcs}
   OUTPUT_SRCS            srcs
   CORE
)

foreach (SUFF IN ITEMS IM RM RD LM)
  string (TOLOWER ${SUFF} suff)
  foreach (RANK RANGE 1 5)

       set_source_files_properties(${CMAKE_CURRENT_BINARY_DIR}/field_${RANK}${suff}_data_module.F90
                                   PROPERTIES COMPILE_OPTIONS $<${HAVE_CUDA}:-cuda>)

  endforeach ()
endforeach ()

## expand non ranksuff sources
field_api_expand_fypp(
   PYTHON_MODULE_DIR      ${PROJECT_SOURCE_DIR}/python_utils
   SOURCE_DIR             ${CMAKE_CURRENT_SOURCE_DIR}
   INPUT_SRCS             ${non_ranksuff_srcs}
   OUTPUT_SRCS            srcs
)

## define object library
field_api_add_object_library(
    LIBNAME        field_api_core
    SRCS           ${srcs}
    LIBRARIES      field_api_debug
)

if(HAVE_GET_VIEW_ABORT)
  target_compile_definitions( field_api_core PRIVATE "-DGET_VIEW_ABORT_DEFAULT_VALUE=.TRUE.")
else()
  target_compile_definitions( field_api_core PRIVATE "-DGET_VIEW_ABORT_DEFAULT_VALUE=.FALSE.")
endif()
if(HAVE_DELAYED)
	target_compile_definitions( field_api_core PRIVATE "-DDELAYED_DEFAULT_VALUE_FROM_CMAKE=.TRUE.")
else()
	target_compile_definitions( field_api_core PRIVATE "-DDELAYED_DEFAULT_VALUE_FROM_CMAKE=.FALSE.")
endif()


set_source_files_properties( ${CMAKE_CURRENT_BINARY_DIR}/dev_alloc_module.F90 PROPERTIES COMPILE_OPTIONS $<${HAVE_CUDA}:-cuda>)
set_source_files_properties( ${CMAKE_CURRENT_BINARY_DIR}/host_alloc_module.F90 PROPERTIES COMPILE_OPTIONS $<${HAVE_CUDA}:-cuda>)

## don't build precision dependent sourcefile
set_source_files_properties( ${CMAKE_CURRENT_BINARY_DIR}/field_module.F90 PROPERTIES HEADER_FILE_ONLY ON)
