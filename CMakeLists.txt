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
project(field_api VERSION 0.3.4 LANGUAGES Fortran)
set(LIBNAME field_api)

## load cmake macros
include( field_api_macros )

## find required packages
ecbuild_find_package(OpenMP COMPONENTS Fortran REQUIRED)

## find fypp
field_api_find_fypp()

## find OpenACC
if( ${CMAKE_VERSION} VERSION_LESS "3.25" )
  if ( FIELD_API_ENABLE_ACC OR (NOT DEFINED FIELD_API_ENABLE_ACC AND (ENABLE_ACC OR NOT DEFINED ENABLE_ACC)) )
    # See https://gitlab.kitware.com/cmake/cmake/-/issues/23691, fixed in CMake 3.25
    # (TL;DR: FindOpenACC sets OpenACC_<LANG>_FOUND correctly but does not set
    #  OpenACC_FOUND unless all three C, CXX, and Fortran have been found - even if
    #  only one language has been requested via COMPONENTS)
    find_package( OpenACC COMPONENTS Fortran )
    if( OpenACC_Fortran_FOUND )
      set( OpenACC_FOUND ON )
    endif()
  endif()
endif()
ecbuild_add_option( FEATURE ACC
                    DEFAULT ON
                    DESCRIPTION "Support for using GPUs with OpenACC"
                    REQUIRED_PACKAGES "OpenACC COMPONENTS Fortran" 
                    CONDITION CMAKE_Fortran_COMPILER_ID MATCHES "PGI|NVHPC")

## set general compiler flags
include( field_api_compile_options )

## choose to build dp or sp or both
ecbuild_add_option( FEATURE SINGLE_PRECISION
	DESCRIPTION "Compile field_api in single precision" DEFAULT ON)
ecbuild_add_option( FEATURE DOUBLE_PRECISION
	DESCRIPTION "Compile field_api in double precision" DEFAULT ON)

set(DEFAULT_PRECISION sp)
if(HAVE_SINGLE_PRECISION)
    list(APPEND fiat_components single)
    list(APPEND precisions sp)
endif()
if(HAVE_DOUBLE_PRECISION)
    set(DEFAULT_PRECISION dp)
    list(APPEND precisions dp)
    list(APPEND fiat_components double)
endif()

## find fiat
field_api_find_fiat_modules()

list(APPEND srcs field_async_module.F90 field_basic_module.F90 field_defaults_module.F90 dev_alloc.c c_malloc.c field_constants_module.F90 field_abort_module.F90 field_statistics_module.F90 field_backtrace.c)

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

## Field Gang
ecbuild_add_option( FEATURE FIELD_GANG
    DESCRIPTION "Enable packed storage of fields"
    DEFAULT ON
    CONDITION (NOT CMAKE_Fortran_COMPILER_ID MATCHES "Cray")
)

check_symbol_exists(backtrace execinfo.h HAVE_BACKTRACE)

## fypp preprocessor flags
if(HAVE_BUDDY_MALLOC)
  list( APPEND fypp_defines "-DUSE_BUDDY_MALLOC")
endif()
if(HAVE_CUDA)
  list( APPEND fypp_defines "-DCUDA")
endif()
if(fiat_FOUND)
  list( APPEND fypp_defines "-DWITH_FIAT")
endif()
if(HAVE_FIELD_GANG)
  list( APPEND fypp_defines "-DHAVE_GANG")
endif()

unset( ranksuff_srcs )
## preprocess fypp files
list( APPEND ranksuff_srcs _shuffle _access _util _array _array_util _factory _gather _data)
if(HAVE_FIELD_GANG)
  list( APPEND ranksuff_srcs _gang )
endif()

## expand ranksuff sources
field_api_expand_fypp_ranksuff(
   PYTHON_MODULE_DIR      ${CMAKE_CURRENT_SOURCE_DIR}
   SOURCE_DIR             ${CMAKE_CURRENT_SOURCE_DIR}
   INPUT_SRCS             ${ranksuff_srcs}
   OUTPUT_SRCS            srcs
)

unset( non_ranksuff_srcs )
list ( APPEND non_ranksuff_srcs dev_alloc_module field_factory_module field_access_module field_array_module field_module 
                      field_shuffle_module field_util_module field_array_util_module field_shuffle_type_module host_alloc_module
                      field_gathscat_module field_gathscat_type_module)
if(HAVE_FIELD_GANG)
  list( APPEND non_ranksuff_srcs field_gang_module )
endif()

## expand non ranksuff sources
field_api_expand_fypp(
   PYTHON_MODULE_DIR      ${CMAKE_CURRENT_SOURCE_DIR}
   SOURCE_DIR             ${CMAKE_CURRENT_SOURCE_DIR}
   INPUT_SRCS             ${non_ranksuff_srcs}
   OUTPUT_SRCS            srcs
)

set_source_files_properties("dev_alloc_module.F90" PROPERTIES COMPILE_OPTIONS $<${HAVE_CUDA}:-cuda>)
set_source_files_properties("host_alloc_module.F90" PROPERTIES COMPILE_OPTIONS $<${HAVE_CUDA}:-cuda>)

## determine lib type
cmake_dependent_option(BUILD_SHARED_LIBS "Dynamically link field_api" ON "NOT HAVE_ACC" OFF)
if( HAVE_ACC )
  ecbuild_warn("OpenACC builds force static linking.")
endif()

list(APPEND prec_srcs field_module.F90 field_array_module.F90 field_array_util_module.F90 field_util_module.F90)
list(REMOVE_ITEM srcs ${prec_srcs})

## build precision independent source files
ecbuild_add_library(
    TARGET ${LIBNAME}
    TYPE OBJECT
    SOURCES ${srcs}
    DEFINITIONS $<$<NOT:${fiat_FOUND}>:${FIELD_API_DEFINITIONS}> $<${fiat_FOUND}:WITH_FIAT>
    PRIVATE_LIBS
       $<${HAVE_ACC}:OpenACC::OpenACC_Fortran>
       $<${fiat_FOUND}:fiat>
       $<${fiat_FOUND}:parkind_${DEFAULT_PRECISION}>
       OpenMP::OpenMP_Fortran
    )
target_include_directories( ${LIBNAME} PRIVATE ${CMAKE_CURRENT_SOURCE_DIR})
set_property(TARGET ${LIBNAME} PROPERTY C_STANDARD 99)
set_target_properties( ${LIBNAME} PROPERTIES Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/include/${LIBNAME} )
target_link_options( ${LIBNAME} PRIVATE $<${HAVE_CUDA}:-cuda> )
if( HAVE_BACKTRACE )
    target_compile_definitions( ${LIBNAME} PRIVATE HAVE_BACKTRACE )
endif()

# set install location for .mod files
install(DIRECTORY ${CMAKE_BINARY_DIR}/include/${LIBNAME} DESTINATION ${CMAKE_INSTALL_INCLUDEDIR})

foreach(prec ${precisions})
  ## add field_api target
  ecbuild_add_library(
      TARGET ${LIBNAME}_${prec}
      SOURCES ${prec_srcs} $<TARGET_OBJECTS:${LIBNAME}>
      DEFINITIONS $<$<NOT:${fiat_FOUND}>:${FIELD_API_DEFINITIONS}>
      PRIVATE_LIBS
         $<${fiat_FOUND}:fiat>
         $<${fiat_FOUND}:parkind_${prec}>
         OpenMP::OpenMP_Fortran
      )
  target_include_directories( ${LIBNAME}_${prec} PRIVATE ${CMAKE_BINARY_DIR}/include/${LIBNAME})
  set_property(TARGET ${LIBNAME}_${prec} PROPERTY C_STANDARD 99)
  set_target_properties( ${LIBNAME}_${prec} PROPERTIES Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/include/${LIBNAME}_${prec} )
  target_link_options( ${LIBNAME}_${prec} PUBLIC $<${HAVE_CUDA}:-cuda> )

  if( HAVE_ACC AND CMAKE_Fortran_COMPILER_ID MATCHES "PGI|NVHPC")
     target_link_options( ${LIBNAME}_${prec} INTERFACE SHELL:${OpenACC_Fortran_FLAGS} )
  endif()

  # export target usage interface
  target_include_directories( ${LIBNAME}_${prec}
                              INTERFACE $<BUILD_INTERFACE:${CMAKE_BINARY_DIR}/include/${LIBNAME}_${prec}> 
                              INTERFACE $<INSTALL_INTERFACE:${CMAKE_INSTALL_INCLUDEDIR}/${LIBNAME}_${prec}> 
                              INTERFACE $<BUILD_INTERFACE:${CMAKE_BINARY_DIR}/include/${LIBNAME}>
                              INTERFACE $<INSTALL_INTERFACE:${CMAKE_INSTALL_INCLUDEDIR}/${LIBNAME}>
                            )

  if( prec MATCHES sp )
      target_compile_definitions( ${LIBNAME}_${prec} PRIVATE PARKIND1_SINGLE )
  endif()
  
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
