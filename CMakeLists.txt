# (C) Copyright 2022- ECMWF.
# (C) Copyright 2022- Meteo-France.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation
# nor does it submit to any jurisdiction.

cmake_minimum_required (VERSION 3.24)

## find ecbuild
find_package( ecbuild 3.7 QUIET HINTS ${CMAKE_BINARY_DIR} ${CMAKE_CURRENT_SOURCE_DIR} ${CMAKE_CURRENT_SOURCE_DIR}/../ecbuild )
if( NOT ecbuild_FOUND )
  include(cmake/field_api_fetchcontent_ecbuild.cmake)
endif()

## find ecbuild toolchain
if( DEFINED ENV{ECBUILD_TOOLCHAIN} AND NOT CMAKE_TOOLCHAIN_FILE)
  set(CMAKE_TOOLCHAIN_FILE ${CMAKE_CURRENT_SOURCE_DIR}/$ENV{ECBUILD_TOOLCHAIN})
endif()

## define project and field_api target name
project(field_api VERSION 0.2.2 LANGUAGES Fortran)
set(LIBNAME field_api)

## find required packages
ecbuild_find_package(OpenMP COMPONENTS Fortran REQUIRED)

## find fypp
find_program(FYPP fypp HINTS ${fypp_ROOT})
if( NOT FYPP)
  include(cmake/field_api_fetchcontent_fypp.cmake)
  set(FYPP ${fypp_SOURCE_DIR}/bin/fypp)
  set(fypp_ROOT ${fypp_SOURCE_DIR}/bin PARENT_SCOPE)
  ecbuild_info("fypp downloaded to: ${FYPP}")
endif()

## find OpenACC
### Workaround for CMake bug (fixed in CMake 3.25)
find_package(OpenACC COMPONENTS Fortran)
if( OpenACC_Fortran_FOUND )
   set( OpenACC_FOUND ON )
endif()
ecbuild_add_option( FEATURE ACC
    DESCRIPTION "OpenACC" DEFAULT ON
    CONDITION OpenACC_FOUND )

## set general compiler flags
include(cmake/field_api_compile_options.cmake)

## choose to build dp or sp or both
ecbuild_add_option( FEATURE SINGLE_PRECISION
	DESCRIPTION "Compile field_api in single precision" DEFAULT ON)
ecbuild_add_option( FEATURE DOUBLE_PRECISION
	DESCRIPTION "Compile field_api in double precision" DEFAULT ON)

set(DEFAULT_PRECISION sp)
if(HAVE_SINGLE_PRECISION)
    list(APPEND precisions sp)
endif()
if(HAVE_DOUBLE_PRECISION)
    set(DEFAULT_PRECISION dp)
    list(APPEND precisions dp)
endif()

## find fiat
ecbuild_find_package(NAME fiat)
if( NOT fiat_FOUND )
  if(NOT UTIL_MODULE_PATH)
    ecbuild_critical("If not building with fiat, then the path for utility modules must be specified")
  endif()

  ecbuild_info( "Checking for FIAT components in ${UTIL_MODULE_PATH}" )

  find_file( ABOR1_PATH abor1.F90 REQUIRED
      HINTS ${UTIL_MODULE_PATH} ${UTIL_MODULE_PATH}/src/fiat/util
  )
  ecbuild_info( "Found ABOR1: ${ABOR1_PATH}" )

  find_file( OML_PATH oml_mod.F90 REQUIRED
      HINTS ${UTIL_MODULE_PATH} ${UTIL_MODULE_PATH}/src/fiat/oml
  )
  ecbuild_info( "Found OML: ${OML_PATH}" )

  find_file( PARKIND1_PATH parkind1.F90 REQUIRED
      HINTS ${UTIL_MODULE_PATH} ${UTIL_MODULE_PATH}/src/parkind
  )
  ecbuild_info( "Found PARKIND1: ${PARKIND1_PATH}" )
  list(APPEND srcs ${ABOR1_PATH} ${OML_PATH} ${PARKIND1_PATH})
endif()

list(APPEND srcs field_basic_module.F90 field_defaults_module.F90 dev_alloc.c c_malloc.c field_constants_module.F90 field_abort_module.F90)

## check for CUDA
include(CheckLanguage)
check_language(CUDA)
ecbuild_add_option( FEATURE CUDA
    DESCRIPTION "CUDA" DEFAULT ON
    CONDITION CMAKE_CUDA_COMPILER AND HAVE_ACC )

## buddy allocator option
ecbuild_add_option( FEATURE BUDDY_MALLOC
    DESCRIPTION "Use buddy allocator for shadow host allocation"
    DEFAULT ON
)

## fypp preprocessor flags
if(HAVE_BUDDY_MALLOC)
  list( APPEND fypp_defines "-DUSE_BUDDY_MALLOC")
endif()
if(HAVE_CUDA)
  list( APPEND fypp_defines "-DCUDA")
endif()

## preprocess fypp files
foreach (SUFF IN ITEMS IM RM RB RD LM)
  string (TOLOWER ${SUFF} suff)
  foreach (RANK RANGE 1 5)
    foreach (FUNC IN ITEMS "" _shuffle _access _util _array_util _gang _factory _gather _data)
      add_custom_command (OUTPUT field_${RANK}${suff}${FUNC}_module.F90
	      COMMAND ${FYPP} -DRANK=${RANK} -DSUFF='${SUFF}' ${fypp_defines} -m os -M ${CMAKE_CURRENT_SOURCE_DIR} -m fieldType 
        ${CMAKE_CURRENT_SOURCE_DIR}/field_RANKSUFF${FUNC}_module.fypp > field_${RANK}${suff}${FUNC}_module.F90
        DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/field_RANKSUFF${FUNC}_module.fypp
        VERBATIM)
      list(APPEND srcs "field_${RANK}${suff}${FUNC}_module.F90")
      set_source_files_properties( "field_${RANK}${suff}${FUNC}_module.F90" PROPERTIES COMPILE_OPTIONS $<${HAVE_CUDA}:-cuda>)
    endforeach ()
  endforeach ()
endforeach ()

foreach (SRC IN ITEMS dev_alloc_module field_factory_module field_access_module field_gang_module field_array_module field_module 
                      field_shuffle_module field_util_module field_array_util_module field_shuffle_type_module host_alloc_module
                      field_gathscat_module field_gathscat_type_module)
  add_custom_command (OUTPUT ${SRC}.F90
	  COMMAND ${FYPP} -m os ${fypp_defines} -M ${CMAKE_CURRENT_SOURCE_DIR} -m fieldType ${CMAKE_CURRENT_SOURCE_DIR}/${SRC}.fypp > ${SRC}.F90
    DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/${SRC}.fypp
    VERBATIM)
  list(APPEND srcs "${SRC}.F90")
endforeach ()
set_source_files_properties("dev_alloc_module.F90" PROPERTIES COMPILE_OPTIONS $<${HAVE_CUDA}:-cuda>)
set_source_files_properties("host_alloc_module.F90" PROPERTIES COMPILE_OPTIONS $<${HAVE_CUDA}:-cuda>)

foreach(prec ${precisions})
  ## add field_api target
  ecbuild_add_library(
      TYPE STATIC
      TARGET ${LIBNAME}_${prec}
      SOURCES ${srcs}
      DEFINITIONS $<$<NOT:${fiat_FOUND}>:${FIELD_API_DEFINITIONS}>
      PUBLIC_LIBS
         $<${HAVE_ACC}:OpenACC::OpenACC_Fortran>
      PRIVATE_LIBS
         $<${fiat_FOUND}:fiat>
         $<${fiat_FOUND}:parkind_${prec}>
         OpenMP::OpenMP_Fortran
      )
  target_include_directories( ${LIBNAME}_${prec} PRIVATE ${CMAKE_CURRENT_SOURCE_DIR})
  set_property(TARGET ${LIBNAME}_${prec} PROPERTY C_STANDARD 99)
  set_target_properties( ${LIBNAME}_${prec} PROPERTIES Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/include/${LIBNAME}_${prec} )
  target_link_options( ${LIBNAME}_${prec} PRIVATE $<${HAVE_CUDA}:-cuda> )

  # export target usage interface
  target_include_directories( ${LIBNAME}_${prec}
                              INTERFACE $<BUILD_INTERFACE:${CMAKE_BINARY_DIR}/include/${LIBNAME}_${prec}> 
                              INTERFACE $<INSTALL_INTERFACE:${CMAKE_INSTALL_INCLUDEDIR}/${LIBNAME}_${prec}> 
                            )
  
  # set install location for .mod files
  install(DIRECTORY ${CMAKE_BINARY_DIR}/include/${LIBNAME}_${prec} DESTINATION ${CMAKE_INSTALL_INCLUDEDIR})
endforeach()

## build tests
ecbuild_add_option(
    FEATURE TESTS
    DEFAULT ON
    DESCRIPTION "Enable field_api testing"
    CONDITION fiat_FOUND
)
if( HAVE_TESTS )
  include(CTest)
  add_subdirectory(tests)
endif()

## export the project
ecbuild_install_project(NAME ${PROJECT_NAME})
