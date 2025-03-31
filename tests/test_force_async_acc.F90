! (C) Copyright 2022- ECMWF.
! (C) Copyright 2022- Meteo-France.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.


program test_force_async

  use field_module
  use field_factory_module
  use parkind1
  use field_abort_module
  use field_defaults_module
  use field_async_module, only: wait_for_async_queue

  implicit none

  class(field_3rb), pointer :: f_ptr => null()
  real(kind=jprb), pointer  :: ptr_cpu(:,:,:)
  real(kind=jprb), pointer  :: ptr_gpu(:,:,:)
  integer                   :: i,j,k
  integer, parameter        :: nstreams = 3
  integer, parameter        :: inner_rank = 1024
  integer, parameter        :: final_rank = 36
  logical                   :: okay(nstreams)
  logical                   :: okay_scalar
  integer                   :: queue, stream, stream_no
  integer                   :: streams(nstreams)
  integer                   :: queues(nstreams)
  integer                   :: blk_size
  integer                   :: buffer_size
  integer                   :: blk_idx
  integer                   :: blk_start, blk_end
  integer                   :: blk_count
  integer                   :: offset
  integer                   :: blk_bounds(2)

  init_pinned_value = .true.
  init_map_devptr = .false.
  okay = .true.
  okay_scalar = .true.

  call field_new(f_ptr, lbounds=[1,1,1], ubounds=[inner_rank,inner_rank,final_rank], persistent=.true.)
  call f_ptr%get_host_data_rdwr(ptr_cpu)
  do k=1,final_rank
    ptr_cpu(:,:,k)=k
  end do 

  ! allocate device "block buffers"
  buffer_size = 12
  blk_count=(final_rank+buffer_size-1)/buffer_size
  call f_ptr%create_device_data(blk_bounds=[1,nstreams*buffer_size])

  ! loop over the blocks and copy data to host
  do blk_idx = 0, blk_count-1
    blk_start=blk_idx*buffer_size+1
    blk_end= min((blk_idx+1)*buffer_size, final_rank)
    blk_bounds(1) = blk_start
    blk_bounds(2) = blk_end
    blk_size = blk_bounds(2) - blk_bounds(1) + 1
    queue = modulo(blk_idx, blk_count)+1
    offset = blk_start-1

    call f_ptr%get_device_data_force(ptr_gpu, blk_bounds=blk_bounds, queue=queue, offset=offset)

      !$acc kernels loop async(queue) deviceptr(ptr_gpu)
      do k = blk_start, blk_end
        do j = 1,inner_rank
          do i=1,inner_rank
            ptr_gpu(i,j,k) = 100 + k
          end do
        end do
      end do
      !$acc end kernels

    call f_ptr%sync_host_force(blk_bounds=blk_bounds, queue=queue, offset=offset)
  end do

  call wait_for_async_queue(1)
  call wait_for_async_queue(2)
  call wait_for_async_queue(3)

  do k=1,final_rank
    do j = 1,inner_rank
      do i=1,inner_rank
        if ( ptr_cpu(i,j,k) /= 100 + k ) then
          okay_scalar = .false.
          call field_abort("error data not updated on host (async copy)")
        end if
      end do
    end do
  end do

end program test_force_async
