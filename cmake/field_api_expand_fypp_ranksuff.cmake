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
# field_api_expand_fypp_ranksuff
# ==============================
# 
# Preprocess a given set of .fypp files over the rank
# and suffix (i.e. field data-type) matrix. ::
# 
#        field_api_expand_fypp_ranksuff( PYTHON_MODULE_DIR <python-util-module-dir> 
#                                        SOURCE_DIR        <source-directory> 
#                                        INPUT_SRCS        <fypp files>
#                                        OUTPUT_SRCS       <processed F90 files> 
#                                        [CORE] )
#
# Input variables
# ---------------
#
# :PYTHON_MODULE_DIR:  Directory containing fieldType.py python module.
# :SOURCE_DIR:         Directory containing .fypp files to be processed.
# :INPUT_SRCS:         List of .fypp files to be processed.
# :OUTPUT_SRCS:        List of processed .F90 files named field_${rank}${suff}_<name>_module.F90
# :CORE:               Flag to determine whether field_${rank}${suff}_module.F90
#                      files are to be generated.
#
##############################################################################

macro( field_api_expand_fypp_ranksuff )

    set( options CORE )
    set( oneValueArgs PYTHON_MODULE_DIR SOURCE_DIR )
    set( multiValueArgs INPUT_SRCS OUTPUT_SRCS )

    cmake_parse_arguments( _PAR "${options}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN} )

    foreach (SUFF IN ITEMS IM RM RD LM)
      string (TOLOWER ${SUFF} suff)
      foreach (RANK RANGE 1 5)

        if( ${_PAR_CORE} )
           foreach (FUNC ${_PAR_INPUT_SRCS} "")
   
             add_custom_command (OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/field_${RANK}${suff}${FUNC}_module.F90
       	      COMMAND ${FYPP} -DRANK=${RANK} -DSUFF='${SUFF}' ${fypp_defines} -m os -M ${_PAR_PYTHON_MODULE_DIR} -m fieldType 
               -DOFFLOAD_MODEL="${FIELD_API_OFFLOAD_MODEL}" -M ${_PAR_PYTHON_MODULE_DIR} -m offload_macros
               ${_PAR_SOURCE_DIR}/field_RANKSUFF${FUNC}_module.fypp > ${CMAKE_CURRENT_BINARY_DIR}/field_${RANK}${suff}${FUNC}_module.F90
               DEPENDS ${_PAR_SOURCE_DIR}/field_RANKSUFF${FUNC}_module.fypp
               VERBATIM)
   
             list(APPEND ${_PAR_OUTPUT_SRCS} ${CMAKE_CURRENT_BINARY_DIR}/field_${RANK}${suff}${FUNC}_module.F90)
           endforeach ()
        else()
           foreach (FUNC ${_PAR_INPUT_SRCS})
   
             add_custom_command (OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/field_${RANK}${suff}${FUNC}_module.F90
       	      COMMAND ${FYPP} -DRANK=${RANK} -DSUFF='${SUFF}' ${fypp_defines} -m os -M ${_PAR_PYTHON_MODULE_DIR} -m fieldType 
               -DOFFLOAD_MODEL="${FIELD_API_OFFLOAD_MODEL}" -M ${_PAR_PYTHON_MODULE_DIR} -m offload_macros
               ${_PAR_SOURCE_DIR}/field_RANKSUFF${FUNC}_module.fypp > ${CMAKE_CURRENT_BINARY_DIR}/field_${RANK}${suff}${FUNC}_module.F90
               DEPENDS ${_PAR_SOURCE_DIR}/field_RANKSUFF${FUNC}_module.fypp
               VERBATIM)
   
             list(APPEND ${_PAR_OUTPUT_SRCS} ${CMAKE_CURRENT_BINARY_DIR}/field_${RANK}${suff}${FUNC}_module.F90)
           endforeach ()
        endif()
      endforeach ()
    endforeach ()

endmacro()
