# (C) Copyright 2022- ECMWF.
# (C) Copyright 2022- Meteo-France.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation
# nor does it submit to any jurisdiction.

macro( field_api_target_add_module_dirs )

    set( options )
    set( oneValueArgs TARGET )
    set( multiValueArgs LIBS )

    cmake_parse_arguments( _PAR "${options}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN} )

    foreach( _LIB ${_PAR_LIBS} )

        target_include_directories( ${_PAR_TARGET}
                                    PRIVATE ${CMAKE_BINARY_DIR}/include/${_LIB}
                                    INTERFACE $<BUILD_INTERFACE:${CMAKE_BINARY_DIR}/include/${_LIB}> 
                                    INTERFACE $<INSTALL_INTERFACE:${CMAKE_INSTALL_INCLUDEDIR}/${_LIB}> 
                                  )
    endforeach()

endmacro()