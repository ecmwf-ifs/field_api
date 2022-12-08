set(TEST_FILES
	delete_device_wrapper.F90
	final_owner.F90
	final_wrapper.F90
	final_wrapper_gpu.F90
	get_stats.F90
	get_view.F90
	init_owner.F90
	init_owner2.F90
	init_owner_gpu.F90
	init_owner_openmp.F90
	init_wrapper.F90
	init_wrapper_gpu.F90
	init_wrapper_lbounds.F90
	init_wrapper_non_contiguous.F90
	pointer_to_owner_wrapper.F90
	wrapper_modify_gpu.F90
	sync_host.F90
	sync_device.F90
	)

foreach(TEST_FILE ${TEST_FILES})
	get_filename_component(TEST_NAME ${TEST_FILE} NAME_WE)
	add_executable(${TEST_NAME}.x ${TEST_FILE})
	target_link_libraries(${TEST_NAME}.x ${LIBNAME})
	add_test(NAME ${TEST_NAME} COMMAND ${TEST_NAME}.x)
endforeach()
