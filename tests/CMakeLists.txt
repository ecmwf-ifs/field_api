# (C) Copyright 2022- ECMWF.
# (C) Copyright 2022- Meteo-France.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation
# nor does it submit to any jurisdiction.

set(LIBNAME_PREC ${LIBNAME}_${DEFAULT_PRECISION})

# set DEV_ALLOC_SIZE for tests
set(dev_alloc_size "268435456") # 256MB

## Host-device ping-pong runner
ecbuild_add_test(
    TARGET main.x
    SOURCES main.F90
    LIBS
        ${LIBNAME_PREC}
        $<${fiat_FOUND}:parkind_${DEFAULT_PRECISION}>
        $<${fiat_FOUND}:fiat>
        OpenMP::OpenMP_Fortran
        $<${HAVE_ACC}:OpenACC::OpenACC_Fortran>
        $<$<BOOL:${HAVE_IO}>:HDF5::HDF5>
    LINKER_LANGUAGE Fortran
	ENVIRONMENT "DEV_ALLOC_SIZE=${dev_alloc_size}"
)

if(HAVE_CUDA)
  if(CMAKE_Fortran_COMPILER_VERSION VERSION_LESS "25.5")
     target_link_options( main.x PRIVATE -gpu=pinned )
  else()
     target_link_options( main.x PRIVATE -gpu=mem:separate:pinnedalloc )
  endif()
endif()
target_compile_definitions( main.x PRIVATE $<${HAVE_OMP_OFFLOAD}:OMPGPU> )

## Unit tests
list(APPEND TEST_FILES
        cpu_to_gpu.F90
        cpu_to_gpu_delayed_init_value.F90
        cpu_to_gpu_init_value.F90
        delayed_by_default.F90
        delete_device_wrapper.F90
        field_0_size_wrapper_get_view.F90
        final_owner.F90
        final_wrapper.F90
        final_wrapper_gpu.F90
        gather_scatter.F90
        gather_scatter_lastdim.F90
        get_dims.F90
        get_stats.F90
        get_view.F90
        get_view_get_device_data.F90
        get_view_when_ndevfresh.F90
        get_view_when_ndevfresh2.F90
        get_view_when_nhstfresh.F90
        get_view_when_unallocated.F90
        get_view_when_uninitialized.F90
        gpu_to_cpu.F90
        gpu_to_cpu_delayed_init_value.F90
        gpu_to_cpu_init_value.F90
        init_owner.F90
        init_owner2.F90
        init_owner_0_size.F90
        init_owner_delayed.F90
        init_owner_delayed_gpu.F90
        init_owner_delayed_gpu2.F90
        init_owner_delayed_gpu3.F90
        init_owner_delayed_init_debug_value.F90
        init_owner_delayed_init_value.F90
        init_owner_delayed_init_value2.F90
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
        not_delayed_by_default.F90
        pointer_to_owner_wrapper.F90
        reshuffle.F90
        reshuffle_lastdim.F90
        resize_owner.F90
        resize_owner2.F90
        sync_device.F90
        sync_host.F90
        test_0_sized_transfer.F90
        test_0_sized_transfer_wrap.F90
        test_async.F90
        test_bc.F90
        test_field1d.F90
        test_field_array.F90
        test_field_array_0_size.F90
        test_field_clone.F90
        test_field_clone_2.F90
        test_field_clone_device.F90
        test_field_clone_null.F90
        test_field_clone_optional.F90
        test_field_delete_on_null.F90
        test_force_methods.F90
        test_get_device_data_bounds.F90
        test_get_device_data_non_contiguous.F90
        test_get_device_data_wronly.F90
        test_host_mem_pool.F90
        test_lastdim.F90
        test_legacy.F90
        test_pinned.F90
        test_sizeof.F90
        test_statistics.F90
        test_sync_device_wronly.F90
        test_wrappernosynconfinal.F90
        wrapper_modify_gpu.F90
	)
if(fiat_FOUND)
  list(APPEND TEST_FILES
          test_crc64.F90 )
endif()

#Place-holder for failing tests
set(FAILING_TEST_FILES
	)

#These tests will call abor1
set(ABOR1_TEST_FILES
	resize_wrapper.F90
	)

if(HAVE_ACC)
  list(APPEND TEST_FILES
       get_device_data_multi_blk_async.F90
       test_force_async_acc.F90
      )
endif()

if(HAVE_IO_SERIAL OR HAVE_IO_PARALLEL)
  list(APPEND HDF5_TEST_FILES
       test_hdf5_wr_parallel_field2d.F90
       test_hdf5_rw_parallel_general.F90)
endif()

if(HAVE_FIELD_GANG)
  list(APPEND TEST_FILES
        test_gang.F90
        test_field_stack_cpu.F90
        test_field_stack_gpu.F90
        test_field_stack_member_ranks.F90
        test_field_stack_simple.F90
      )
endif()

if(HAVE_GET_VIEW_ABORT)
	list(REMOVE_ITEM TEST_FILES
		get_view_when_ndevfresh.F90
		get_view_when_ndevfresh2.F90
		get_view_when_unallocated.F90
	)
	list(APPEND ABOR1_TEST_FILES
		get_view_when_ndevfresh.F90
		get_view_when_ndevfresh2.F90
		get_view_when_unallocated.F90
	)
endif()

if(HAVE_DELAYED)
	list(APPEND ABOR1_TEST_FILES
		is_delayed_by_default.F90
	)
else()
	list(APPEND TEST_FILES
		is_delayed_by_default.F90
	)
endif()

if(HAVE_DELAYED AND HAVE_GET_VIEW_ABORT)
	list(REMOVE_ITEM TEST_FILES
		get_view_get_device_data.F90
		get_view_when_uninitialized.F90
	) 
	list(APPEND ABOR1_TEST_FILES
		get_view_get_device_data.F90
		get_view_when_uninitialized.F90
	)
endif()

#Set OMP_NUM_THREADS for tests
set(omp_num_threads 8)


if(HAVE_IO_PARALLEL)
foreach(TEST_FILE ${HDF5_TEST_FILES})
    get_filename_component(TEST_NAME ${TEST_FILE} NAME_WE)
    ecbuild_add_test(
        TARGET ${TEST_NAME}.x
        SOURCES ${TEST_FILE}
        LIBS
           ${LIBNAME_PREC}
           $<${fiat_FOUND}:parkind_${DEFAULT_PRECISION}>
           $<${fiat_FOUND}:fiat>
           OpenMP::OpenMP_Fortran
           MPI::MPI_Fortran
           $<${HAVE_ACC}:OpenACC::OpenACC_Fortran>
           $<$<BOOL:${HAVE_IO}>:HDF5::HDF5>
        DEFINITIONS
           $<${HAVE_FIELD_GANG}:HAVE_FIELD_GANG>
        LINKER_LANGUAGE Fortran
        MPI 2
        OMP ${omp_num_threads}
        ENVIRONMENT "DEV_ALLOC_SIZE=${dev_alloc_size}"
    )

    set_target_properties(${TEST_NAME}.x
        PROPERTIES Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/include/tests
    )

    if(HAVE_CUDA)
      if(CMAKE_Fortran_COMPILER_VERSION VERSION_LESS "25.5")
         target_link_options( ${TEST_NAME}.x PRIVATE -gpu=pinned )
      else()
         target_link_options( ${TEST_NAME}.x PRIVATE -gpu=mem:separate:pinnedalloc )
      endif()
    endif()
    target_compile_definitions( ${TEST_NAME}.x PRIVATE
                                $<${HAVE_OMP_OFFLOAD}:OMPGPU>
                                $<${HAVE_MPI}:HAVE_MPI>
                                $<${HAVE_CUDA}:_CUDA> )

    if( DEFAULT_PRECISION MATCHES sp )
       target_compile_definitions( ${TEST_NAME}.x PRIVATE PARKIND1_SINGLE )
    endif()
endforeach()
endif()

foreach(TEST_FILE ${TEST_FILES})
    get_filename_component(TEST_NAME ${TEST_FILE} NAME_WE)
    ecbuild_add_test(
        TARGET ${TEST_NAME}.x
        SOURCES ${TEST_FILE}
        LIBS
           ${LIBNAME_PREC}
           $<${fiat_FOUND}:parkind_${DEFAULT_PRECISION}>
           $<${fiat_FOUND}:fiat>
           OpenMP::OpenMP_Fortran
           $<${HAVE_ACC}:OpenACC::OpenACC_Fortran>
           $<$<BOOL:${HAVE_IO}>:HDF5::HDF5>
        DEFINITIONS
           $<${HAVE_FIELD_GANG}:HAVE_FIELD_GANG>
        LINKER_LANGUAGE Fortran
        OMP ${omp_num_threads}
		ENVIRONMENT "DEV_ALLOC_SIZE=${dev_alloc_size}"
    )

    set_target_properties(${TEST_NAME}.x
        PROPERTIES Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/include/tests
    )

    if(HAVE_CUDA)
      if(CMAKE_Fortran_COMPILER_VERSION VERSION_LESS "25.5")
         target_link_options( ${TEST_NAME}.x PRIVATE -gpu=pinned )
      else()
         target_link_options( ${TEST_NAME}.x PRIVATE -gpu=mem:separate:pinnedalloc )
      endif()
    endif()
    target_compile_definitions( ${TEST_NAME}.x PRIVATE
                                $<$<BOOL:${HAVE_IO}>:HDF5::HDF5>
                                $<${HAVE_OMP_OFFLOAD}:OMPGPU>
                                $<${HAVE_CUDA}:_CUDA> )

    if( DEFAULT_PRECISION MATCHES sp )
       target_compile_definitions( ${TEST_NAME}.x PRIVATE PARKIND1_SINGLE )
    endif()
endforeach()

foreach(FAILING_TEST_FILE ${FAILING_TEST_FILES})
    get_filename_component(FAILING_TEST_NAME ${FAILING_TEST_FILE} NAME_WE)
    add_executable(${FAILING_TEST_NAME}.x ${FAILING_TEST_FILE})
    target_link_libraries(${FAILING_TEST_NAME}.x PRIVATE ${LIBNAME_PREC} $<${fiat_FOUND}:parkind_${DEFAULT_PRECISION} fiat>)
    target_link_libraries(${FAILING_TEST_NAME}.x PRIVATE OpenMP::OpenMP_Fortran)
    target_link_libraries(${FAILING_TEST_NAME}.x PRIVATE $<${HAVE_ACC}:OpenACC::OpenACC_Fortran>)
    target_link_libraries(${FAILING_TEST_NAME}.x PRIVATE $<$<BOOL:${HAVE_IO}>:HDF5::HDF5>)
    set_target_properties(${FAILING_TEST_NAME}.x PROPERTIES LINKER_LANGUAGE Fortran)
    add_test(NAME ${FAILING_TEST_NAME} COMMAND ${FAILING_TEST_NAME}.x)
    set_property(TEST ${FAILING_TEST_NAME} PROPERTY WILL_FAIL TRUE)
    set_property(TEST ${FAILING_TEST_NAME} PROPERTY ENVIRONMENT "OMP_NUM_THREADS=${omp_num_threads};DEV_ALLOC_SIZE=${dev_alloc_size}")
    if(HAVE_CUDA)
      if(CMAKE_Fortran_COMPILER_VERSION VERSION_LESS "25.5")
         target_link_options( ${FAILING_TEST_NAME}.x PRIVATE -gpu=pinned )
      else()
         target_link_options( ${FAILING_TEST_NAME}.x PRIVATE -gpu=mem:separate:pinnedalloc )
      endif()
    endif()
    target_compile_definitions( ${FAILING_TEST_NAME}.x PRIVATE $<${HAVE_CUDA}:_CUDA> )
endforeach()

foreach(ABOR1_TEST_FILE ${ABOR1_TEST_FILES})
    get_filename_component(ABOR1_TEST_NAME ${ABOR1_TEST_FILE} NAME_WE)
    add_executable(${ABOR1_TEST_NAME}.x ${ABOR1_TEST_FILE})
    target_link_libraries(${ABOR1_TEST_NAME}.x PRIVATE ${LIBNAME_PREC} $<${fiat_FOUND}:parkind_${DEFAULT_PRECISION} fiat>)
    target_link_libraries(${ABOR1_TEST_NAME}.x PRIVATE OpenMP::OpenMP_Fortran)
    target_link_libraries(${ABOR1_TEST_NAME}.x PRIVATE $<${HAVE_ACC}:OpenACC::OpenACC_Fortran>)
    target_link_libraries(${ABOR1_TEST_NAME}.x PRIVATE $<$<BOOL:${HAVE_IO}>:HDF5::HDF5>)
    set_target_properties(${ABOR1_TEST_NAME}.x PROPERTIES LINKER_LANGUAGE Fortran)
    add_test(NAME ${ABOR1_TEST_NAME} COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/abor1catcher.sh "./${ABOR1_TEST_NAME}.x")
    set_property(TEST ${ABOR1_TEST_NAME} PROPERTY ENVIRONMENT "OMP_NUM_THREADS=${omp_num_threads};DEV_ALLOC_SIZE=${dev_alloc_size}")
    if(HAVE_CUDA)
      if(CMAKE_Fortran_COMPILER_VERSION VERSION_LESS "25.5")
         target_link_options( ${ABOR1_TEST_NAME}.x PRIVATE -gpu=pinned )
      else()
         target_link_options( ${ABOR1_TEST_NAME}.x PRIVATE -gpu=mem:separate:pinnedalloc )
      endif()
    endif()
    target_compile_definitions( ${ABOR1_TEST_NAME}.x PRIVATE $<${HAVE_CUDA}:_CUDA> )
endforeach()

if(fiat_FOUND)
## Mixed precision unit-test
  ecbuild_add_test(
      TARGET  init_wrapper_mixed_precision.x
      SOURCES init_wrapper_mixed_precision.F90
      LIBS
        ${LIBNAME}_sp
        $<${fiat_FOUND}:parkind_sp>
        $<${fiat_FOUND}:fiat>
        OpenMP::OpenMP_Fortran
        $<${HAVE_ACC}:OpenACC::OpenACC_Fortran>
        $<$<BOOL:${HAVE_IO}>:HDF5::HDF5>
      LINKER_LANGUAGE Fortran
      CONDITION ${HAVE_SINGLE_PRECISION}
  )
endif()

## Test presence of GPUs
add_executable(check_gpu_num.x check_gpu_num.F90)
target_link_libraries(check_gpu_num.x PRIVATE $<${HAVE_ACC}:OpenACC::OpenACC_Fortran> OpenMP::OpenMP_Fortran)
set_target_properties(check_gpu_num.x PROPERTIES LINKER_LANGUAGE Fortran)
target_compile_definitions( check_gpu_num.x PRIVATE $<${HAVE_OMP_OFFLOAD}:OMPGPU> )
if(HAVE_OMP_OFFLOAD)
  target_link_options( check_gpu_num.x PRIVATE SHELL:${OpenMP_Fortran_FLAGS} )
endif()

if(HAVE_ACC OR HAVE_OMP_OFFLOAD)
   add_test(NAME confirm_gpu_exists COMMAND check_gpu_num.x)
else()
   add_test(NAME confirm_gpu_does_not_exist COMMAND check_gpu_num.x)
   set_property(TEST confirm_gpu_does_not_exist PROPERTY WILL_FAIL TRUE)
endif()
