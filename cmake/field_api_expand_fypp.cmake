# (C) Copyright 2022- ECMWF.
# (C) Copyright 2022- Meteo-France.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation
# nor does it submit to any jurisdiction.

macro( field_api_expand_fypp )

    set( options )
    set( oneValueArgs PYTHON_MODULE_DIR SOURCE_DIR )
    set( multiValueArgs INPUT_SRCS OUTPUT_SRCS )

    cmake_parse_arguments( _PAR "${options}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN} )

    foreach (SRC ${_PAR_INPUT_SRCS} )

      add_custom_command (OUTPUT ${SRC}.F90
    	  COMMAND ${FYPP} -m os ${fypp_defines} -M ${_PAR_PYTHON_MODULE_DIR} -m fieldType ${_PAR_SOURCE_DIR}/${SRC}.fypp > ${SRC}.F90
        DEPENDS ${_PAR_SOURCE_DIR}/${SRC}.fypp
        VERBATIM)
      list(APPEND ${_PAR_OUTPUT_SRCS} ${SRC}.F90)

    endforeach ()

endmacro()