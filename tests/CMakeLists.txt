# (C) Copyright 2022- ECMWF.
# (C) Copyright 2022- Meteo-France.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation
# nor does it submit to any jurisdiction.

set(LIBNAME_PREC ${LIBNAME}_${DEFAULT_PRECISION})

## Host-device ping-pong runner
ecbuild_add_test(
    TARGET main.x
    SOURCES main.F90
    LIBS
    ${LIBNAME_PREC}
    parkind_${DEFAULT_PRECISION}
       fiat
       OpenMP::OpenMP_Fortran
    LINKER_LANGUAGE Fortran
)
target_link_options( main.x PRIVATE $<${HAVE_CUDA}:-cuda;-gpu=pinned> )
target_compile_definitions( main.x PRIVATE $<${HAVE_CUDA}:_CUDA> )

## Unit tests
list(APPEND TEST_FILES
        gather_scatter_lastdim.F90
        reshuffle_lastdim.F90
        test_lastdim.F90
        test_statistics.F90
        test_sizeof.F90
        test_bc.F90
        reshuffle.F90
	test_wrappernosynconfinal.F90
	test_field1d.F90
	test_pinned.F90
	async_host.F90
	cpu_to_gpu.F90
	cpu_to_gpu_delayed_init_value.F90
	cpu_to_gpu_init_value.F90
	delete_device_wrapper.F90
	final_owner.F90
	final_wrapper.F90
	final_wrapper_gpu.F90
	gather_scatter.F90
	get_dims.F90
	get_stats.F90
	get_view.F90
	get_view_get_device_data.F90
	get_view_when_nhstfresh.F90
	get_view_when_uninitialized.F90
	init_owner.F90
	init_owner2.F90
	init_owner_0_size.F90
	init_owner_delayed.F90
	init_owner_delayed_gpu.F90
	init_owner_delayed_init_debug_value.F90
	init_owner_delayed_init_value.F90
	init_owner_gpu.F90
	init_owner_init_debug_value.F90
	init_owner_init_debug_value_gpu.F90
	init_owner_init_delayed_debug_value_gpu.F90
	init_owner_init_delayed_value_gpu.F90
	init_owner_init_value.F90
	init_owner_openmp.F90
	init_wrapper.F90
	init_wrapper_gpu.F90
	init_wrapper_lbounds.F90
	init_wrapper_non_contiguous.F90
	init_wrapper_non_contiguous_multi.F90
	no_transfer_get_device.F90
	no_transfer_get_host.F90
	pointer_to_owner_wrapper.F90
	resize_owner.F90
	resize_owner2.F90
	sync_device.F90
	sync_host.F90
	test_crc64.F90
	wrapper_modify_gpu.F90
	test_gang.F90
	)

#Place-holder for failing tests
set(FAILING_TEST_FILES
	)

#These tests will call abor1
set(ABOR1_TEST_FILES
	get_view_when_ndevfresh.F90
	get_view_when_unallocated.F90
	resize_wrapper.F90
	)

#Set OMP_NUM_THREADS for tests
set(omp_num_threads 8)

foreach(TEST_FILE ${TEST_FILES})
	get_filename_component(TEST_NAME ${TEST_FILE} NAME_WE)
    ecbuild_add_test(
        TARGET ${TEST_NAME}.x
        SOURCES ${TEST_FILE}
        LIBS
           ${LIBNAME_PREC}
           parkind_${DEFAULT_PRECISION}
           fiat
           OpenMP::OpenMP_Fortran
        LINKER_LANGUAGE Fortran
        OMP ${omp_num_threads}
    )

    set_target_properties(${TEST_NAME}.x
        PROPERTIES Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/include/tests
    )

    target_link_options( ${TEST_NAME}.x PRIVATE $<${HAVE_CUDA}:-cuda;-gpu=pinned> )
    target_compile_definitions( ${TEST_NAME}.x PRIVATE $<${HAVE_CUDA}:_CUDA> )
endforeach()

foreach(FAILING_TEST_FILE ${FAILING_TEST_FILES})
	get_filename_component(FAILING_TEST_NAME ${FAILING_TEST_FILE} NAME_WE)
	add_executable(${FAILING_TEST_NAME}.x ${FAILING_TEST_FILE})
	target_link_libraries(${FAILING_TEST_NAME}.x ${LIBNAME_PREC} parkind_${DEFAULT_PRECISION} fiat OpenMP::OpenMP_Fortran)
	set_target_properties(${FAILING_TEST_NAME}.x PROPERTIES LINKER_LANGUAGE Fortran)
	add_test(NAME ${FAILING_TEST_NAME} COMMAND ${FAILING_TEST_NAME}.x)
	set_property(TEST ${FAILING_TEST_NAME} PROPERTY WILL_FAIL TRUE)
	set_property(TEST ${FAILING_TEST_NAME} PROPERTY ENVIRONMENT "OMP_NUM_THREADS=${omp_num_threads}")
    target_link_options( ${FAILING_TEST_NAME}.x PRIVATE $<${HAVE_CUDA}:-cuda;-gpu=pinned> )
    target_compile_definitions( ${FAILING_TEST_NAME}.x PRIVATE $<${HAVE_CUDA}:_CUDA> )
endforeach()

foreach(ABOR1_TEST_FILE ${ABOR1_TEST_FILES})
	get_filename_component(ABOR1_TEST_NAME ${ABOR1_TEST_FILE} NAME_WE)
	add_executable(${ABOR1_TEST_NAME}.x ${ABOR1_TEST_FILE})
	target_link_libraries(${ABOR1_TEST_NAME}.x ${LIBNAME_PREC} parkind_${DEFAULT_PRECISION} fiat OpenMP::OpenMP_Fortran)
	set_target_properties(${ABOR1_TEST_NAME}.x PROPERTIES LINKER_LANGUAGE Fortran)
	add_test(NAME ${ABOR1_TEST_NAME} COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/abor1catcher.sh "./${ABOR1_TEST_NAME}.x")
	set_property(TEST ${ABOR1_TEST_NAME} PROPERTY ENVIRONMENT "OMP_NUM_THREADS=${omp_num_threads}")
    target_link_options( ${ABOR1_TEST_NAME}.x PRIVATE $<${HAVE_CUDA}:-cuda;-gpu=pinned> )
    target_compile_definitions( ${ABOR1_TEST_NAME}.x PRIVATE $<${HAVE_CUDA}:_CUDA> )
endforeach()

## Mixed precision unit-test
ecbuild_add_test(
    TARGET  init_wrapper_mixed_precision.x
    SOURCES init_wrapper_mixed_precision.F90
    LIBS
      ${LIBNAME}_sp
      fiat
      parkind_sp
      OpenMP::OpenMP_Fortran
    LINKER_LANGUAGE Fortran
    CONDITION ${HAVE_SINGLE_PRECISION}
)

if (HAVE_SINGLE_PRECISION)
  # don't need gpu=pinned link flag because this test only has wrapper fields
  target_link_options( init_wrapper_mixed_precision.x PRIVATE $<${HAVE_CUDA}:-cuda> )
endif ()

## Test presence of GPUs
add_executable(check_gpu_num.x check_gpu_num.F90)
target_link_libraries(check_gpu_num.x PRIVATE $<${HAVE_ACC}:OpenACC::OpenACC_Fortran>)
set_target_properties(check_gpu_num.x PROPERTIES LINKER_LANGUAGE Fortran)

if(HAVE_ACC)
   add_test(NAME confirm_gpu_exists COMMAND check_gpu_num.x)
else()
   add_test(NAME confirm_gpu_does_not_exist COMMAND check_gpu_num.x)
   set_property(TEST confirm_gpu_does_not_exist PROPERTY WILL_FAIL TRUE)
endif()
