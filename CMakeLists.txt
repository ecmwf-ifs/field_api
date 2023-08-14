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

## define project and field_api target
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

get_filename_component (Fortran_COMPILER_NAME ${CMAKE_Fortran_COMPILER} NAME)

option(USE_OPENACC "Necessary to use field api on GPU" OFF)

if (USE_OPENACC)

  if (CMAKE_Fortran_COMPILER_ID STREQUAL "PGI" 
   OR CMAKE_Fortran_COMPILER_ID STREQUAL "NVIDIA" 
   OR CMAKE_Fortran_COMPILER_ID STREQUAL "NVHPC")
    set (CMAKE_Fortran_FLAGS "-acc=gpu -Mlarge_arrays -gopt -gpu=cc70 -Minfo=accel,all,ccff")
  endif ()

endif ()


find_package (fiat REQUIRED) 

set (CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -I${fiat_BASE_DIR}/module/fiat -I${fiat_BASE_DIR}/include/fiat")

if (fiat_HAVE_DOUBLE_PRECISION)
  set (CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -I${fiat_BASE_DIR}/module/parkind_dp")
elseif (fiat_HAVE_SINGLE_PRECISION)
  set (CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -I${fiat_BASE_DIR}/module/parkind_sp")
endif ()

set (SOURCES field_basic_module.F90 field_init_debug_value_module.F90 dev_alloc.c)

foreach (SUFF IN ITEMS IM RM RB RD LM)
  string (TOLOWER ${SUFF} suff)
  foreach (RANK RANGE 2 5)
    foreach (FUNC IN ITEMS "" _gathscat _access _util _array_util)
      add_custom_command (OUTPUT field_${RANK}${suff}${FUNC}_module.F90
	      COMMAND ${FYPP} -DRANK=${RANK} -DSUFF='${SUFF}' -m os -M ${CMAKE_CURRENT_SOURCE_DIR} -m fieldType 
        ${CMAKE_CURRENT_SOURCE_DIR}/field_RANKSUFF${FUNC}_module.fypp > field_${RANK}${suff}${FUNC}_module.F90
        DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/field_RANKSUFF${FUNC}_module.fypp
        VERBATIM)
      list (APPEND SOURCES "field_${RANK}${suff}${FUNC}_module.F90")
    endforeach ()
  endforeach ()
endforeach ()

foreach (SRC IN ITEMS dev_alloc_module field_factory_module field_access_module 
         field_array_module field_module field_gathscat_module field_util_module field_array_util_module)
  add_custom_command (OUTPUT ${SRC}.F90
	  COMMAND ${FYPP} -m os -M ${CMAKE_CURRENT_SOURCE_DIR} -m fieldType ${CMAKE_CURRENT_SOURCE_DIR}/${SRC}.fypp > ${SRC}.F90
    DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/${SRC}.fypp
    VERBATIM)
  list (APPEND SOURCES "${SRC}.F90")
endforeach ()

add_library (${LIBNAME} ${SOURCES})
set_property (TARGET ${LIBNAME} PROPERTY C_STANDARD 99)

target_link_libraries (${LIBNAME} PUBLIC OpenMP::OpenMP_Fortran)

target_include_directories (${LIBNAME} PRIVATE ${CMAKE_CURRENT_SOURCE_DIR})

if (OpenACC_Fortran_FOUND AND USE_OPENACC)
  target_link_libraries (${LIBNAME} PUBLIC OpenACC::OpenACC_Fortran)
endif ()

add_executable (main.x main.F90)
target_link_libraries (main.x PUBLIC ${LIBNAME} -L${fiat_BASE_DIR}/lib64 -L${fiat_BASE_DIR}/lib -lfiat -Wl,-rpath,${fiat_BASE_DIR}/lib64 -Wl,-rpath,${fiat_BASE_DIR}/lib)
enable_testing ()
add_subdirectory (tests)

install(TARGETS ${LIBNAME} DESTINATION ${CMAKE_INSTALL_PREFIX}/lib)
install(DIRECTORY ${CMAKE_Fortran_MODULE_DIRECTORY} DESTINATION ${CMAKE_INSTALL_PREFIX})
