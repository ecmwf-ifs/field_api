#COPYRIGHT Meteo-France 2023
set(TEST_FILES
	async_host.F90
	cpu_to_gpu.F90
	cpu_to_gpu_delayed_init_value.F90
	cpu_to_gpu_init_value.F90
	delete_device_wrapper.F90
	final_owner.F90
	final_wrapper.F90
	final_wrapper_gpu.F90
	gather_scatter.F90
	get_stats.F90
	get_view.F90
	get_view_get_device_data.F90
	init_owner.F90
	init_owner2.F90
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
	no_transfer_get_device.F90
	no_transfer_get_host.F90
	pointer_to_owner_wrapper.F90
	sync_device.F90
	sync_host.F90
	wrapper_modify_gpu.F90
	)

set(FAILING_TEST_FILES
	get_device_data_get_view.F90
	)

set (CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/tests/modules)
include_directories(${CMAKE_BINARY_DIR}/modules)

foreach(TEST_FILE ${TEST_FILES})
	get_filename_component(TEST_NAME ${TEST_FILE} NAME_WE)
	add_executable(${TEST_NAME}.x ${TEST_FILE})
	target_link_libraries(${TEST_NAME}.x ${LIBNAME} -L${fiat_BASE_DIR}/lib64 -L${fiat_BASE_DIR}/lib -lfiat -Wl,-rpath,${fiat_BASE_DIR}/lib64 -Wl,-rpath,${fiat_BASE_DIR}/lib)
	add_test(NAME ${TEST_NAME} COMMAND ${TEST_NAME}.x)
endforeach()

foreach(FAILING_TEST_FILE ${FAILING_TEST_FILES})
	get_filename_component(FAILING_TEST_NAME ${FAILING_TEST_FILE} NAME_WE)
	add_executable(${FAILING_TEST_NAME}.x ${FAILING_TEST_FILE})
	target_link_libraries(${FAILING_TEST_NAME}.x ${LIBNAME} -L${fiat_BASE_DIR}/lib64 -L${fiat_BASE_DIR}/lib -lfiat -Wl,-rpath,${fiat_BASE_DIR}/lib64 -Wl,-rpath,${fiat_BASE_DIR}/lib)
	add_test(NAME ${FAILING_TEST_NAME} COMMAND ${FAILING_TEST_NAME}.x)
	set_property(TEST ${FAILING_TEST_NAME} PROPERTY WILL_FAIL TRUE)
endforeach()
