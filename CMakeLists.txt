# (C) Copyright 2022- ECMWF.
# (C) Copyright 2022- Meteo-France.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation
# nor does it submit to any jurisdiction.

cmake_minimum_required (VERSION 3.25)

## find ecbuild
find_package( ecbuild 3.8 QUIET HINTS ${CMAKE_BINARY_DIR} ${CMAKE_CURRENT_SOURCE_DIR} ${CMAKE_CURRENT_SOURCE_DIR}/../ecbuild )
if( NOT ecbuild_FOUND )
  include(cmake/field_api_fetchcontent_ecbuild.cmake)
endif()

## define project and field_api target name
project(field_api VERSION 1.0 LANGUAGES Fortran)
set(LIBNAME field_api)
set (CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/modules)

## find required packages
ecbuild_find_package(OpenMP COMPONENTS Fortran REQUIRED)

## find fypp
find_program(FYPP fypp HINTS ${fypp_ROOT})
if( NOT FYPP)
include(FetchContent)

FetchContent_Declare(
   fypp
   GIT_REPOSITORY https://github.com/aradi/fypp
   GIT_TAG 3.1
)
FetchContent_MakeAvailable(fypp)

set(FYPP ${fypp_SOURCE_DIR}/bin/fypp)
ecbuild_info("fypp downloaded to: ${FYPP}")
endif()

## find OpenACC
# Workaround for FindOpenACC not setting OpenACC_FOUND
find_package(OpenACC COMPONENTS Fortran)
if(OpenACC_Fortran_FOUND)
  set(OpenACC_FOUND ON)
endif()
ecbuild_add_option( FEATURE ACC
    DESCRIPTION "OpenACC" DEFAULT ON
    CONDITION OpenACC_FOUND )

if(ACC AND CMAKE_Fortran_COMPILER_ID MATCHES PGI|NVIDIA|NVHPC)
  set(CMAKE_Fortran_FLAGS "-acc=gpu -Mlarge_arrays -gopt -gpu=cc70 -Minfo=accel,all,ccff")
endif ()

## find fiat
ecbuild_add_option(
    FEATURE FIELD_API_FIAT_BUILD
    DEFAULT ON
    DESCRIPTION "Import oml_mod, parkind and abor1 from fiat"
)
if( ${HAVE_FIELD_API_FIAT_BUILD} )
  ecbuild_find_package(fiat REQUIRED) 
else()
  if(NOT FIELD_API_UTIL_MODULE_PATH)
    ecbuild_critical("If not building with fiat, then the path for utility modules must be specified")
  endif()

  ecbuild_info( "Checking for FIAT components in ${FIELD_API_UTIL_MODULE_PATH}" )

  find_file( ABOR1_PATH abor1.F90 REQUIRED
      HINTS ${FIELD_API_UTIL_MODULE_PATH} ${FIELD_API_UTIL_MODULE_PATH}/src/fiat/util
  )
  ecbuild_info( "Found ABOR1: ${ABOR1_PATH}" )

  find_file( OML_PATH oml_mod.F90 REQUIRED
      HINTS ${FIELD_API_UTIL_MODULE_PATH} ${FIELD_API_UTIL_MODULE_PATH}/src/fiat/oml
  )
  ecbuild_info( "Found OML: ${OML_PATH}" )

  find_file( PARKIND1_PATH parkind1.F90 REQUIRED
      HINTS ${FIELD_API_UTIL_MODULE_PATH} ${FIELD_API_UTIL_MODULE_PATH}/src/parkind
  )
  ecbuild_info( "Found PARKIND1: ${PARKIND1_PATH}" )
  list(APPEND srcs ${ABOR1_PATH} ${OML_PATH} ${PARKIND1_PATH})
endif()

list(APPEND srcs field_basic_module.F90 field_init_debug_value_module.F90 dev_alloc.c)

foreach (SUFF IN ITEMS IM RM RB RD LM)
  string (TOLOWER ${SUFF} suff)
  foreach (RANK RANGE 2 5)
    foreach (FUNC IN ITEMS "" _gathscat _access _util _array_util)
      add_custom_command (OUTPUT field_${RANK}${suff}${FUNC}_module.F90
	      COMMAND ${FYPP} -n -DRANK=${RANK} -DSUFF='${SUFF}' -m os -M ${CMAKE_CURRENT_SOURCE_DIR} -m fieldType 
        ${CMAKE_CURRENT_SOURCE_DIR}/field_RANKSUFF${FUNC}_module.fypp > field_${RANK}${suff}${FUNC}_module.F90
        DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/field_RANKSUFF${FUNC}_module.fypp
        VERBATIM)
      list(APPEND srcs "field_${RANK}${suff}${FUNC}_module.F90")
    endforeach ()
  endforeach ()
endforeach ()

foreach (SRC IN ITEMS dev_alloc_module field_factory_module field_access_module 
         field_array_module field_module field_gathscat_module field_util_module field_array_util_module)
  add_custom_command (OUTPUT ${SRC}.F90
	  COMMAND ${FYPP} -n -m os -M ${CMAKE_CURRENT_SOURCE_DIR} -m fieldType ${CMAKE_CURRENT_SOURCE_DIR}/${SRC}.fypp > ${SRC}.F90
    DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/${SRC}.fypp
    VERBATIM)
  list(APPEND srcs "${SRC}.F90")
endforeach ()

## add field_api target
foreach(prec dp sp)
  ecbuild_add_library(
      TARGET ${LIBNAME}_${prec}
      SOURCES ${srcs}
      PRIVATE_LIBS
         $<${HAVE_FIELD_API_FIAT_BUILD}:fiat>
         $<${HAVE_FIELD_API_FIAT_BUILD}:parkind_${prec}>
      PUBLIC_LIBS
         OpenMP::OpenMP_Fortran
         $<${HAVE_ACC}:OpenACC::OpenACC_Fortran>
      )
  set_property(TARGET ${LIBNAME}_${prec} PROPERTY C_STANDARD 99)
  target_include_directories(${LIBNAME} PRIVATE ${CMAKE_CURRENT_SOURCE_DIR})
  
  install(TARGETS ${LIBNAME}_${prec} DESTINATION ${CMAKE_INSTALL_PREFIX}/lib)
  install(DIRECTORY ${CMAKE_Fortran_MODULE_DIRECTORY} DESTINATION ${CMAKE_INSTALL_PREFIX})
endforeach()

add_executable (main.x main.F90)
target_link_libraries (main.x PUBLIC ${LIBNAME} -L${fiat_BASE_DIR}/lib64 -L${fiat_BASE_DIR}/lib -lfiat -Wl,-rpath,${fiat_BASE_DIR}/lib64 -Wl,-rpath,${fiat_BASE_DIR}/lib)
enable_testing ()
add_subdirectory (tests)


