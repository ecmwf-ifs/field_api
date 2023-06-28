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


foreach (SUFF IN ITEMS IM RM RB RD LM)
  string (TOLOWER ${SUFF} suff)
  foreach (RANK RANGE 2 5)
    foreach (FUNC IN ITEMS "" _gathscat _access)
    add_custom_command(
        OUTPUT field_${RANK}${suff}${FUNC}_module.F90
	COMMAND fypp -DRANK=${RANK} -DSUFF='${SUFF}' -m os -M ${CMAKE_CURRENT_SOURCE_DIR} -m fieldType ${CMAKE_CURRENT_SOURCE_DIR}/field_ranksuff${FUNC}_module.fypp > field_${RANK}${suff}${FUNC}_module.F90
	DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/field_ranksuff${FUNC}_module.fypp
	VERBATIM)
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

add_library(${LIBNAME}  

            field_2im_module.F90          field_3im_module.F90          field_4im_module.F90          field_5im_module.F90 
            field_2rm_module.F90          field_3rm_module.F90          field_4rm_module.F90          field_5rm_module.F90 
            field_2rb_module.F90          field_3rb_module.F90          field_4rb_module.F90          field_5rb_module.F90 
            field_2rd_module.F90          field_3rd_module.F90          field_4rd_module.F90          field_5rd_module.F90 
            field_2lm_module.F90          field_3lm_module.F90          field_4lm_module.F90          field_5lm_module.F90 

            field_2im_gathscat_module.F90 field_3im_gathscat_module.F90 field_4im_gathscat_module.F90 field_5im_gathscat_module.F90 
            field_2rm_gathscat_module.F90 field_3rm_gathscat_module.F90 field_4rm_gathscat_module.F90 field_5rm_gathscat_module.F90 
            field_2rb_gathscat_module.F90 field_3rb_gathscat_module.F90 field_4rb_gathscat_module.F90 field_5rb_gathscat_module.F90 
            field_2rd_gathscat_module.F90 field_3rd_gathscat_module.F90 field_4rd_gathscat_module.F90 field_5rd_gathscat_module.F90 
            field_2lm_gathscat_module.F90 field_3lm_gathscat_module.F90 field_4lm_gathscat_module.F90 field_5lm_gathscat_module.F90 

            field_2im_access_module.F90   field_3im_access_module.F90   field_4im_access_module.F90   field_5im_access_module.F90 
            field_2rm_access_module.F90   field_3rm_access_module.F90   field_4rm_access_module.F90   field_5rm_access_module.F90 
            field_2rb_access_module.F90   field_3rb_access_module.F90   field_4rb_access_module.F90   field_5rb_access_module.F90 
            field_2rd_access_module.F90   field_3rd_access_module.F90   field_4rd_access_module.F90   field_5rd_access_module.F90 
            field_2lm_access_module.F90   field_3lm_access_module.F90   field_4lm_access_module.F90   field_5lm_access_module.F90 

            field_module.F90 field_basic_module.F90 abor1.F90 parkind1.F90 dev_alloc_module.F90 dev_alloc.c 
            field_factory_module.F90 field_access_module.F90 field_array_wrapper_module.F90 field_gathscat_module.F90)

target_link_libraries(${LIBNAME} PUBLIC OpenMP::OpenMP_Fortran)

target_include_directories(${LIBNAME} PRIVATE ${CMAKE_CURRENT_SOURCE_DIR})


if(OpenACC_Fortran_FOUND AND USE_OPENACC)
	target_link_libraries(${LIBNAME} PUBLIC OpenACC::OpenACC_Fortran)
endif()

add_executable(main.x main.F90)
target_link_libraries(main.x PUBLIC ${LIBNAME})

enable_testing()
add_subdirectory(tests)
