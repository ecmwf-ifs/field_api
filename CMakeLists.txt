cmake_minimum_required (VERSION 3.15)

project (field_api)
enable_language (Fortran)
set (LIBNAME field_api)
set (CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/modules)
set (CMAKE_BUILD_TYPE "Debug")

get_filename_component (Fortran_COMPILER_NAME ${CMAKE_Fortran_COMPILER} NAME)

option(USE_OPENACC "Necessary to use field api on GPU" OFF)

if (USE_OPENACC)

  if (CMAKE_Fortran_COMPILER_ID STREQUAL "PGI" 
   OR CMAKE_Fortran_COMPILER_ID STREQUAL "NVIDIA" 
   OR CMAKE_Fortran_COMPILER_ID STREQUAL "NVHPC")
    set (CMAKE_Fortran_FLAGS "-acc=gpu -Mlarge_arrays -gopt -gpu=cc70 -Minfo=accel,all,intensity,ccff")
  endif ()

endif ()

find_package (OpenMP)

if (NOT OpenMP_Fortran_FOUND)
  message (FATAL_ERROR "Could not find OpenMP support.")
endif ()

find_package (fiat REQUIRED) 

set (CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -I${fiat_BASE_DIR}/module/fiat -I${fiat_BASE_DIR}/include/fiat")
set (CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -L${fiat_BASE_DIR}/lib64 -lfiat -Wl,-rpath,${fiat_BASE_DIR}/lib64")

if (fiat_HAVE_DOUBLE_PRECISION)
  set (CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -I${fiat_BASE_DIR}/module/parkind_dp")
elseif (fiat_HAVE_SINGLE_PRECISION)
  set (CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -I${fiat_BASE_DIR}/module/parkind_sp")
endif ()

find_program (FYPP fypp)
if (NOT FYPP)
  message (FATAL_ERROR "Could not find fypp.")
endif()

set (SOURCES
    field_module.F90 field_basic_module.F90 dev_alloc_module.F90 dev_alloc.c 
    field_factory_module.F90 field_access_module.F90 field_array_module.F90 
    field_gathscat_module.F90)

foreach (SUFF IN ITEMS IM RM RB RD LM)
  string (TOLOWER ${SUFF} suff)
  foreach (RANK RANGE 2 5)
    foreach (FUNC IN ITEMS "" _gathscat _access)
      add_custom_command (OUTPUT field_${RANK}${suff}${FUNC}_module.F90
        COMMAND fypp -DRANK=${RANK} -DSUFF='${SUFF}' -m os -M ${CMAKE_CURRENT_SOURCE_DIR} -m fieldType 
        ${CMAKE_CURRENT_SOURCE_DIR}/field_RANKSUFF${FUNC}_module.fypp > field_${RANK}${suff}${FUNC}_module.F90
        DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/field_RANKSUFF${FUNC}_module.fypp
        VERBATIM)
      list (APPEND SOURCES "field_${RANK}${suff}${FUNC}_module.F90")
    endforeach ()
  endforeach ()
endforeach ()

foreach (SRC IN ITEMS dev_alloc_module field_factory_module field_access_module field_array_module field_module field_gathscat_module)
  add_custom_command (OUTPUT ${SRC}.F90
    COMMAND fypp -m os -M ${CMAKE_CURRENT_SOURCE_DIR} -m fieldType ${CMAKE_CURRENT_SOURCE_DIR}/${SRC}.fypp > ${SRC}.F90
    DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/${SRC}.fypp
    VERBATIM)
endforeach ()

add_library (${LIBNAME} ${SOURCES})
set_property (TARGET ${LIBNAME} PROPERTY C_STANDARD 99)

target_link_libraries (${LIBNAME} PUBLIC OpenMP::OpenMP_Fortran)

target_include_directories (${LIBNAME} PRIVATE ${CMAKE_CURRENT_SOURCE_DIR})

if (OpenACC_Fortran_FOUND AND USE_OPENACC)
  target_link_libraries (${LIBNAME} PUBLIC OpenACC::OpenACC_Fortran)
endif ()

add_executable (main.x main.F90)
target_link_libraries (main.x PUBLIC ${LIBNAME})
enable_testing ()
add_subdirectory (tests)

install(TARGETS ${LIBNAME} DESTINATION ${CMAKE_INSTALL_PREFIX}/lib)
install(DIRECTORY ${CMAKE_Fortran_MODULE_DIRECTORY} DESTINATION ${CMAKE_INSTALL_PREFIX})

