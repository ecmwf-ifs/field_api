cmake_minimum_required(VERSION 3.15)

project(field_api)
enable_language (Fortran)
set(LIBNAME field_api)
set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/modules)

get_filename_component (Fortran_COMPILER_NAME ${CMAKE_Fortran_COMPILER} NAME)
if (Fortran_COMPILER_NAME MATCHES "nvfortran")
	set (CMAKE_Fortran_FLAGS "-acc=gpu -Mcuda -Mlarge_arrays")
endif()

find_package(OpenMP)
if(NOT OpenMP_Fortran_FOUND)
	MESSAGE(FATAL_ERROR "Could not find OpenMP support.")
endif()

set(CMAKE_BUILD_TYPE "Debug")

# Pre-process: .fpp -> .f90 via Fypp
find_program(FYPP fypp)
if (NOT FYPP)
	MESSAGE(FATAL_ERROR "Could not find fypp.")
endif()
add_custom_command(
	OUTPUT field_module.F90
	COMMAND fypp -m os ${CMAKE_CURRENT_SOURCE_DIR}/field_module.fypp > field_module.F90
	DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/field_module.fypp
	VERBATIM)

add_library(${LIBNAME} field_module.F90 abor1.F90 parkind1.F90)
target_link_libraries(${LIBNAME} PUBLIC OpenMP::OpenMP_Fortran)

add_executable(main.x main.F90)
target_link_libraries(main.x PUBLIC ${LIBNAME})

add_executable(cuda.x cuda.F90)
target_link_libraries(cuda.x PUBLIC ${LIBNAME})

enable_testing()
add_subdirectory(tests)
