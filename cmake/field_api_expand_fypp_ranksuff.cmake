# (C) Copyright 2022- ECMWF.
# (C) Copyright 2022- Meteo-France.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation
# nor does it submit to any jurisdiction.

macro( field_api_expand_fypp_ranksuff )

    set( options )
    set( oneValueArgs PYTHON_MODULE_DIR SOURCE_DIR )
    set( multiValueArgs INPUT_SRCS OUTPUT_SRCS )

    cmake_parse_arguments( _PAR "${options}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN} )

    foreach (SUFF IN ITEMS IM RM RD LM)
      string (TOLOWER ${SUFF} suff)
      foreach (RANK RANGE 1 5)
        foreach (FUNC ${_PAR_INPUT_SRCS} "")
          add_custom_command (OUTPUT field_${RANK}${suff}${FUNC}_module.F90
    	      COMMAND ${FYPP} -DRANK=${RANK} -DSUFF='${SUFF}' ${fypp_defines} -m os -M ${_PAR_PYTHON_MODULE_DIR} -m fieldType 
            ${_PAR_SOURCE_DIR}/field_RANKSUFF${FUNC}_module.fypp > field_${RANK}${suff}${FUNC}_module.F90
            DEPENDS ${_PAR_SOURCE_DIR}/field_RANKSUFF${FUNC}_module.fypp
            VERBATIM)
          list(APPEND ${_PAR_OUTPUT_SRCS} field_${RANK}${suff}${FUNC}_module.F90)
          set_source_files_properties(field_${RANK}${suff}${FUNC}_module.F90 PROPERTIES COMPILE_OPTIONS $<${HAVE_CUDA}:-cuda>)
        endforeach ()
      endforeach ()
    endforeach ()

endmacro()