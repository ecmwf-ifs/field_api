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
# Preprocess a given set of .fypp files. ::
# 
#        field_api_expand_fypp( PYTHON_MODULE_DIR <python-util-module-dir> 
#                               SOURCE_DIR        <source-directory> 
#                               INPUT_SRCS        <fypp files>
#                               OUTPUT_SRCS       <processed F90 files> )
#
# Input variables
# ---------------
#
# :PYTHON_MODULE_DIR:  Directory containing fieldType.py python module.
# :SOURCE_DIR:         Directory containing .fypp files to be processed.
# :INPUT_SRCS:         List of .fypp files to be processed.
# :OUTPUT_SRCS:        List of processed .F90 files.
#
##############################################################################

macro( field_api_expand_fypp )

    set( options )
    set( oneValueArgs PYTHON_MODULE_DIR SOURCE_DIR )
    set( multiValueArgs INPUT_SRCS OUTPUT_SRCS )

    cmake_parse_arguments( _PAR "${options}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN} )

    foreach (SRC ${_PAR_INPUT_SRCS} )

      add_custom_command (OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/${SRC}.F90
    	  COMMAND ${FYPP} -m os ${fypp_defines} -M ${_PAR_PYTHON_MODULE_DIR} -m fieldType
                        -DOFFLOAD_MODEL="${FIELD_API_OFFLOAD_MODEL}" -M ${_PAR_PYTHON_MODULE_DIR} -m offload_macros
                        ${_PAR_SOURCE_DIR}/${SRC}.fypp > ${CMAKE_CURRENT_BINARY_DIR}/${SRC}.F90
        DEPENDS ${_PAR_SOURCE_DIR}/${SRC}.fypp
        VERBATIM)

      list(APPEND ${_PAR_OUTPUT_SRCS} ${CMAKE_CURRENT_BINARY_DIR}/${SRC}.F90)

    endforeach ()

endmacro()
