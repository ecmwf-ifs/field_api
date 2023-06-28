cmake_minimum_required(VERSION 3.15)

project(field_api)
enable_language (Fortran)
set(LIBNAME field_api)
set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/modules)
set(CMAKE_BUILD_TYPE "Debug")

get_filename_component (Fortran_COMPILER_NAME ${CMAKE_Fortran_COMPILER} NAME)
if (Fortran_COMPILER_NAME MATCHES "nvfortran")
	set (CMAKE_Fortran_FLAGS "-Mlarge_arrays")
endif()

option(USE_OPENACC "Necessary to use field api on GPU" OFF)
find_package(OpenACC)
if(NOT OpenACC_Fortran_FOUND)
	set(USE_OPENACC OFF)
endif()

find_package(OpenMP)
if(NOT OpenMP_Fortran_FOUND)
	MESSAGE(FATAL_ERROR "Could not find OpenMP support.")
endif()


# Pre-process: .fpp -> .f90 via Fypp

find_program(FYPP fypp)
if (NOT FYPP)
	MESSAGE(FATAL_ERROR "Could not find fypp.")
endif()

set (SOURCES
    field_module.F90 field_basic_module.F90 abor1.F90 parkind1.F90 dev_alloc_module.F90 dev_alloc.c 
    field_factory_module.F90 field_access_module.F90 field_array_wrapper_module.F90 field_gathscat_module.F90)


foreach (SUFF IN ITEMS IM RM RB RD LM)
  string (TOLOWER ${SUFF} suff)
  foreach (RANK RANGE 2 5)
    foreach (FUNC IN ITEMS "" _gathscat _access)
      add_custom_command(
        OUTPUT field_${RANK}${suff}${FUNC}_module.F90
        COMMAND fypp -DRANK=${RANK} -DSUFF='${SUFF}' -m os -M ${CMAKE_CURRENT_SOURCE_DIR} -m fieldType ${CMAKE_CURRENT_SOURCE_DIR}/field_RANKSUFF${FUNC}_module.fypp > field_${RANK}${suff}${FUNC}_module.F90
        DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/field_RANKSUFF${FUNC}_module.fypp
        VERBATIM)
      list (APPEND SOURCES "field_${RANK}${suff}${FUNC}_module.F90")
    endforeach ()
  endforeach ()
endforeach ()

foreach (SRC IN ITEMS dev_alloc_module field_factory_module field_access_module field_array_wrapper_module field_module field_gathscat_module)
  add_custom_command(
	OUTPUT ${SRC}.F90
	COMMAND fypp -m os -M ${CMAKE_CURRENT_SOURCE_DIR} -m fieldType ${CMAKE_CURRENT_SOURCE_DIR}/${SRC}.fypp > ${SRC}.F90
	DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/${SRC}.fypp
	VERBATIM)
endforeach ()

add_library(${LIBNAME} ${SOURCES})

target_link_libraries(${LIBNAME} PUBLIC OpenMP::OpenMP_Fortran)

target_include_directories(${LIBNAME} PRIVATE ${CMAKE_CURRENT_SOURCE_DIR})


if(OpenACC_Fortran_FOUND AND USE_OPENACC)
	target_link_libraries(${LIBNAME} PUBLIC OpenACC::OpenACC_Fortran)
endif()

add_executable(main.x main.F90)
target_link_libraries(main.x PUBLIC ${LIBNAME})

enable_testing()
add_subdirectory(tests)
