# (C) Copyright 2022- ECMWF.
# (C) Copyright 2022- Meteo-France.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation
# nor does it submit to any jurisdiction.


list( APPEND ranksuff_srcs _shuffle _gather)
list( APPEND non_ranksuff_srcs field_shuffle_module field_shuffle_type_module field_gathscat_module field_gathscat_type_module)


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

## expand unified collapse templates for COLLAPSE=1 and COLLAPSE=2
foreach (SUFF IN ITEMS IM RM RD LM)
  string (TOLOWER ${SUFF} suff)
  foreach (RANK RANGE 1 5)
    foreach (COLLAPSE IN ITEMS 1 2)
      foreach (FUNC IN ITEMS _shuffle _gather)
        add_custom_command (
          OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/field_${RANK}${suff}${FUNC}_collapse${COLLAPSE}_module.F90
          COMMAND ${FYPP} -DRANK=${RANK} -DSUFF='${SUFF}' -DCOLLAPSE=${COLLAPSE} ${fypp_defines} -m os -M ${PROJECT_SOURCE_DIR}/python_utils -m fieldType
            -DOFFLOAD_MODEL="${FIELD_API_OFFLOAD_MODEL}" -M ${PROJECT_SOURCE_DIR}/python_utils -m offload_macros
            ${CMAKE_CURRENT_SOURCE_DIR}/field_RANKSUFF${FUNC}_collapseCOLLAPSE_module.fypp
            > ${CMAKE_CURRENT_BINARY_DIR}/field_${RANK}${suff}${FUNC}_collapse${COLLAPSE}_module.F90
          DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/field_RANKSUFF${FUNC}_collapseCOLLAPSE_module.fypp
          VERBATIM)
        list(APPEND srcs ${CMAKE_CURRENT_BINARY_DIR}/field_${RANK}${suff}${FUNC}_collapse${COLLAPSE}_module.F90)
      endforeach ()
    endforeach ()
  endforeach ()
endforeach ()

foreach (COLLAPSE IN ITEMS 1 2)
  add_custom_command (
    OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/field_gathscat_collapse${COLLAPSE}_module.F90
    COMMAND ${FYPP} -DCOLLAPSE=${COLLAPSE} ${fypp_defines} -m os -M ${PROJECT_SOURCE_DIR}/python_utils -m fieldType
      -DOFFLOAD_MODEL="${FIELD_API_OFFLOAD_MODEL}" -M ${PROJECT_SOURCE_DIR}/python_utils -m offload_macros
      ${CMAKE_CURRENT_SOURCE_DIR}/field_gathscat_collapseCOLLAPSE_module.fypp
      > ${CMAKE_CURRENT_BINARY_DIR}/field_gathscat_collapse${COLLAPSE}_module.F90
    DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/field_gathscat_collapseCOLLAPSE_module.fypp
    VERBATIM)
  list(APPEND srcs ${CMAKE_CURRENT_BINARY_DIR}/field_gathscat_collapse${COLLAPSE}_module.F90)

  add_custom_command (
    OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/field_gathscat_collapse${COLLAPSE}_type_module.F90
    COMMAND ${FYPP} -DCOLLAPSE=${COLLAPSE} ${fypp_defines} -m os -M ${PROJECT_SOURCE_DIR}/python_utils -m fieldType
      -DOFFLOAD_MODEL="${FIELD_API_OFFLOAD_MODEL}" -M ${PROJECT_SOURCE_DIR}/python_utils -m offload_macros
      ${CMAKE_CURRENT_SOURCE_DIR}/field_gathscat_collapseCOLLAPSE_type_module.fypp
      > ${CMAKE_CURRENT_BINARY_DIR}/field_gathscat_collapse${COLLAPSE}_type_module.F90
    DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/field_gathscat_collapseCOLLAPSE_type_module.fypp
    VERBATIM)
  list(APPEND srcs ${CMAKE_CURRENT_BINARY_DIR}/field_gathscat_collapse${COLLAPSE}_type_module.F90)
endforeach ()

## define object library
field_api_add_object_library(
    LIBNAME        field_api_shuffle
    SRCS           ${srcs}
    LIBRARIES      field_api_core field_api_debug field_api_util field_api_factory field_api_defaults_${DEFAULT_PRECISION}
)
