# (C) Copyright 2022- ECMWF.
# (C) Copyright 2022- Meteo-France.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation
# nor does it submit to any jurisdiction.

list( APPEND srcs hdf5_common_module.F90 ) 
list( APPEND ranksuff_srcs _hdf5)
list( APPEND non_ranksuff_srcs field_hdf5_module )

## expand ranksuff sources
field_api_expand_fypp_ranksuff(
   PYTHON_MODULE_DIR      ${PROJECT_SOURCE_DIR}/python_utils
   SOURCE_DIR             ${CMAKE_CURRENT_SOURCE_DIR}
   INPUT_SRCS             ${ranksuff_srcs}
   OUTPUT_SRCS            srcs
)

## expand non ranksuff sources
field_api_expand_fypp(
   PYTHON_MODULE_DIR      ${PROJECT_SOURCE_DIR}/python_utils
   SOURCE_DIR             ${CMAKE_CURRENT_SOURCE_DIR}
   INPUT_SRCS             ${non_ranksuff_srcs}
   OUTPUT_SRCS            srcs
)

## define object library
field_api_add_object_library(
    LIBNAME        field_api_io
    SRCS           ${srcs}
    LIBRARIES      field_api_core field_api_debug
)

## don't build precision dependent sourcefile
set_source_files_properties( ${CMAKE_CURRENT_BINARY_DIR}/field_hdf5_module.F90 PROPERTIES HEADER_FILE_ONLY ON)

