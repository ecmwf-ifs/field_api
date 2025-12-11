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
project(field_api VERSION 0.3.8 LANGUAGES Fortran)
set(LIBNAME field_api)

## load cmake macros
include( field_api_macros )

## find required packages
ecbuild_find_package(OpenMP COMPONENTS Fortran REQUIRED)

## find fypp
field_api_find_fypp()

## determine GPU offload model
field_api_get_offload_model()

## set general compiler flags
field_api_compile_options()

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

ecbuild_add_option( FEATURE MPI
                    DEFAULT OFF
                    DESCRIPTION "Support for MPI distributed parallelism"
                    REQUIRED_PACKAGES "MPI COMPONENTS Fortran" )

## IO using HDF5
ecbuild_add_option( FEATURE IO_SERIAL
    DESCRIPTION "Use serial HDF5 to read and write FieldAPI variables"
    REQUIRED_PACKAGES "HDF5 COMPONENTS Fortran"
    DEFAULT OFF
)

ecbuild_add_option( FEATURE IO_PARALLEL
    DESCRIPTION "Use HDF5 (rank defined by MPI) to read and write FieldAPI variables"
    REQUIRED_PACKAGES "HDF5 COMPONENTS Fortran"
    CONDITION HAVE_MPI 
    DEFAULT OFF
)

if( HAVE_IO_SERIAL OR HAVE_IO_PARALLEL )
    option( HAVE_IO "HAVE_IO" TRUE)
else()
    option( HAVE_IO "HAVE_IO" FALSE)
endif()

## find FIAT or at least its components 
field_api_find_fiat_modules()

## Field Gang
ecbuild_add_option( FEATURE FIELD_GANG
    DESCRIPTION "Enable packed storage of fields"
    DEFAULT ON
)

##Get_view abort
ecbuild_add_option( FEATURE GET_VIEW_ABORT
	DESCRIPTION "Enable this option to make get_view abort when data are not on CPU"
	DEFAULT ON
)

##Delayed
ecbuild_add_option( FEATURE DELAYED
	DESCRIPTION "Enable this option to delayed by default the allocation of field owner"
	DEFAULT OFF
)


## fypp preprocessor flags
if(HAVE_CUDA)
  list( APPEND fypp_defines "-DWITH_HIC")
endif()
if(fiat_FOUND)
  list( APPEND fypp_defines "-DWITH_FIAT")
endif()
if(HAVE_FIELD_GANG)
  list( APPEND fypp_defines "-DHAVE_GANG")
endif()

## build precision independent srcs
add_subdirectory(src)

## determine lib type
set( SHARED_LIBS_FOR_OFFLOAD_DEFAULT OFF )
if( CMAKE_Fortran_COMPILER_ID MATCHES "PGI|NVHPC" AND
    CMAKE_Fortran_COMPILER_VERSION VERSION_GREATER_EQUAL 25.7.0 )
    if( DEFINED BUILD_SHARED_LIBS )
      set( SHARED_LIBS_FOR_OFFLOAD_DEFAULT ${BUILD_SHARED_LIBS} )
    elseif( DEFINED FIELD_API_BUILD_SHARED_LIBS )
      set( SHARED_LIBS_FOR_OFFLOAD_DEFAULT ${FIELD_API_BUILD_SHARED_LIBS} )
    else()
      set( SHARED_LIBS_FOR_OFFLOAD_DEFAULT ON )
    endif()
endif()
cmake_dependent_option(BUILD_SHARED_LIBS "Dynamically link field_api" ON "NOT (HAVE_ACC OR HAVE_OMP_OFFLOAD)" ${SHARED_LIBS_FOR_OFFLOAD_DEFAULT})

if( (HAVE_ACC OR HAVE_OMP_OFFLOAD) AND NOT ${SHARED_LIBS_FOR_OFFLOAD_DEFAULT} )
  ecbuild_warn("Enabling GPU offload forces static linking.")
endif()

## specify precision dependent sources that must be compiled twice
list(APPEND prec_srcs
    ${CMAKE_CURRENT_BINARY_DIR}/src/core/field_module.F90
    ${CMAKE_CURRENT_BINARY_DIR}/src/util/field_array_module.F90
    ${CMAKE_CURRENT_BINARY_DIR}/src/util/field_array_util_module.F90
    ${CMAKE_CURRENT_BINARY_DIR}/src/util/field_util_module.F90)

if( HAVE_IO )
  list(APPEND prec_srcs
      ${CMAKE_CURRENT_BINARY_DIR}/src/io/field_hdf5_module.F90)
endif()

list(APPEND obj_libs
        field_api_core
        field_api_debug
        field_api_factory
        field_api_util
        field_api_shuffle
)

if( HAVE_FIELD_GANG )
   list(APPEND obj_libs field_api_buffer)
endif()

if( HAVE_IO )
   list(APPEND obj_libs field_api_io)
endif()

## add field_api targets
foreach(prec ${precisions})
  ecbuild_add_library(
      TARGET ${LIBNAME}_${prec}
      SOURCES ${prec_srcs}
      OBJECTS
         ${obj_libs}
      PRIVATE_LIBS
         $<${fiat_FOUND}:fiat>
         $<${fiat_FOUND}:parkind_${prec}>
         OpenMP::OpenMP_Fortran
         $<$<BOOL:${HAVE_IO}>:HDF5::HDF5>
         $<${HAVE_IO_PARALLEL}:MPI::MPI_Fortran>
      )

  set_property(TARGET ${LIBNAME}_${prec} PROPERTY C_STANDARD 99)
  set_target_properties( ${LIBNAME}_${prec} PROPERTIES Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/include/${LIBNAME}_${prec} )
  target_link_options( ${LIBNAME}_${prec} PUBLIC $<${HAVE_CUDA}:-cuda> )

  if( HAVE_ACC AND CMAKE_Fortran_COMPILER_ID MATCHES "PGI|NVHPC")
     target_link_options( ${LIBNAME}_${prec} INTERFACE SHELL:${OpenACC_Fortran_FLAGS} )
  endif()

  if( HAVE_OMP_OFFLOAD )
     target_link_options( ${LIBNAME}_${prec} INTERFACE SHELL:${OpenMP_Fortran_FLAGS} )
  endif()

  # export target usage interface
  target_include_directories( ${LIBNAME}_${prec}
                              INTERFACE $<BUILD_INTERFACE:${CMAKE_BINARY_DIR}/include/${LIBNAME}_${prec}> 
                              INTERFACE $<INSTALL_INTERFACE:${CMAKE_INSTALL_INCLUDEDIR}/${LIBNAME}_${prec}> 
                            )

  # add sub-component module directories to target
  field_api_target_add_module_dirs(
    TARGET     ${LIBNAME}_${prec}
    LIBS       ${obj_libs}
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
)

if( HAVE_TESTS )
  include(CTest)
  add_subdirectory(tests)
endif()

## export the project
ecbuild_install_project(NAME ${PROJECT_NAME})
