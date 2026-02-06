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
# field_api_add_object_library
# ============================
#
# Create an object library from a given set of objects and/or sources. ::
#
#        field_api_add_object_library( LIBNAME       <name>
#                                      OBJECTS       <object-libraries>
#                                      SRCS          <sourcefiles>
#                                      DEFINITIONS   <compile-definitions>
#                                      LIBARIES      <libraries> )
#
# Note that an object library is not a full cmake target, for example
# object libraries never lead to a .so/.a file.
#
# Input variables
# ---------------
#
# :LIBNAME:       Library name.
# :PREC:          Precision for which to build object library.
# :OBJECTS:       Object libraries to include. Only target
#                 objects will be included, other link-time
#                 information is not propagated.
# :SRCS:          Sources to compile.
# :DEFINITIONS:   Preprocessor definitions.
# :LIBRARIES:     Libraries to link against.
#
##############################################################################

macro(field_api_add_object_library)

    set( options )
    set( oneValueArgs LIBNAME PREC )
    set( multiValueArgs OBJECTS SRCS DEFINITIONS LIBRARIES )

    cmake_parse_arguments( _PAR "${options}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN} )

    if( NOT DEFINED _PAR_PREC )
       set( _PAR_PREC ${DEFAULT_PRECISION} )
    endif()

    ecbuild_add_library(
        TARGET ${_PAR_LIBNAME}
        TYPE   OBJECT
        OBJECTS ${_PAR_OBJECTS}
        SOURCES ${_PAR_SRCS}
        DEFINITIONS
           ${_PAR_DEFINITIONS}
           $<$<NOT:${fiat_FOUND}>:${FIELD_API_DEFINITIONS}>
           $<${fiat_FOUND}:WITH_FIAT>
           ${FIELD_API_OFFLOAD_DEFINITIONS}
        PRIVATE_LIBS
           ${_PAR_LIBRARIES}
           $<${HAVE_ACC}:OpenACC::OpenACC_Fortran>
           $<${fiat_FOUND}:fiat>
           $<${fiat_FOUND}:parkind_${_PAR_PREC}>
           $<${HAVE_MPI}:MPI::MPI_Fortran>
           $<${HAVE_HIPFORT}:hipfort::hip>
           OpenMP::OpenMP_Fortran
           $<$<BOOL:${HAVE_IO}>:HDF5::HDF5>
        )

    target_include_directories( ${_PAR_LIBNAME} PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}
                                                INTERFACE $<BUILD_INTERFACE:${CMAKE_BINARY_DIR}/include/${_PAR_LIBNAME}> )

    set_property(TARGET ${_PAR_LIBNAME} PROPERTY C_STANDARD 99)
    set_target_properties( ${_PAR_LIBNAME} PROPERTIES Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/include/${_PAR_LIBNAME} )
    target_link_options( ${_PAR_LIBNAME} PRIVATE $<${HAVE_CUDA}:-cuda> )

    # set install location for .mod files
    install(DIRECTORY ${CMAKE_BINARY_DIR}/include/${_PAR_LIBNAME} DESTINATION ${CMAKE_INSTALL_INCLUDEDIR})

endmacro()
