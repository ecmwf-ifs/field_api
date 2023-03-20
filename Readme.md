# Field API

Field API aims to ease the management and the transfer of data between CPUs and
GPUs for the Météo-France/ECMWF software.

The API is using fypp heavily to generate the code for several types and
dimensions. It might look complicated, but if you are just using the API then
you should not worry about it.

This library should be considered highly experimental.

# Compilation

Field API can be compiled as an external library or just dropped into the
codebase. To compile it you need:
- a compiler with OpenMP and OpenACC and GPU support;
- cmake (>= 3.15);
- fypp;

```
mkdir build
cd build
cmake ..
make
ctest #Optional, will run the tests
```

The library has been tested with the nvfortran compiler (former PGI) 22.11 and
23.1

# Field API types

Field API can encapsulate arrays of different types (REAL, INTEGER, LOGICAL)
and different dimensions (2D, 3D, 4D). Adding new dimensions would be trivial
and just a matter of editing the field\_definitions.fypp file. Adding the
encapsulation of new type would be easy if they are similar to the ones already
encapsulated.

Field API can be used to encapsulate data when used by a single thread or with
multiple threads. By default (persistent option unset or set to true), the last
dimension is used to store the thread number, and then each thread will access
only a single dimension of the array through a view.

```
SUBROUTINE SUB()
TYPE(FIELD_2D_OWNER) :: FW
TYPE(FIELD_2D_VIEW_PTR) :: V

!Will create a field with the first dimension going from 1 to 10 and second from 1 to OMP_NUM_THREADS
CALL FW%INIT(/1,1/, /10,1/)

DO IBLK=1,NBLKS
  V => FW%GET_VIEW(IBLK)
  !do stuff with v
ENDDO
  
CALL FW%FINALIZE()
```

Furthermore field API provides two way of encapsulating the data: wrappers
and owners

## WRAPPER

The field API wrappers (eg. FIELD\_2D\_WRAPPER) provide a way to encapsulate
data which was already allocated before entering a part of the code which uses
field API. It is really just adding a wrapper around an array.

For instance, let say there are some data used in the code that has nothing to
do with GPUs. The data are allocated there and used there. But at some point,
maybe deeper in the code, those data are in fact needed on GPU. Instead of
having to declare a field API object high in the call stack, one could simply
declare a field api wrapper when needed.

```
SUBROUTINE SUB(MYDATA)
INTEGER, INTENT(INOUT) :: MYDATA(:,:)
TYPE(FIELD_2D_WRAPPER) :: FW

!Wrap MYDATA into field wrapper FW
CALL FW%INIT(MYDATA)

!do stuff

CALL FW%FINALIZE()

!MYDATA is still accessible
MYDATA(1,2) = 7
```

## OWNER

The field API owners (eg. FIELD\_2D\_OWNER) provide a way to declare and
allocate  data to be used on CPU and GPU. The data is allocated by the API, the 
user doesn't have to allocate data by itself. Similarly, the user doesn't 
deallocate the data by himself, it is done by the API. When creating a owner,
the user will need to provide the two arrays used to specify the lower and
upper bounds of the array that will be created by field api.

```
SUBROUTINE SUB()
TYPE(FIELD_2D_OWNER) :: FW

!Allocate data with field API 
!The allocated data will have a first dimension
!going from 1 to 10 and a second from 0 to 10.
CALL FW%INIT(/1,0/, /10,10/, PERSISTENT=.FALSE.)

!do stuff

CALL FW%FINALIZE()
!The data has now be freed on CPU and GPU and cannot be accessed anymore
```

### Delayed allocations

Field owners also provide a way to delay the allocation of data. The user can
ask a field owner to be created without allocating the data. The allocation
would then happen only if the data would be requested at some point, later in
the program. It can be useful if one doesn't want to waste memory on data that
might be only conditionally used. But please keep in mind, that allocating data
can be slow and will slow down the program if done during a computation heavy
part of the code.

```
SUBROUTINE SUB(MYTEST)
LOGICAL, INTENT(IN) :: MYTEST
TYPE(FIELD_2D_OWNER) :: FW

!Declare a field owner, no allocation will happen here
CALL FW%INIT(/1,0/, /10,10/, PERSISTENT=.FALSE., DELAYED=.TRUE.)

IF (MYTEST) THEN
!do stuff with FW
!allocation wil happen here
ENDIF

CALL FW%FINALIZE()
!The data will be freed if MYTEST was true, otherwise there are no data to deallocate
```

## Asynchronism

By default all data transfers are synchronous. So every call to the subroutines
GET\_HOST\_DATA, GET\_DEVICE\_DATA, SYNC\_HOST, SYNC\_DEVICE will stop the
program until the data are actually transfered. But sometimes it is possible to
interleave the data transfer with the computations. To do so you can add the
QUEUE parameter when calling the aforementioned subroutines. With this QUEUE
parameter the user will specify on which queue he wants the data transfer to
happen, and the subroutines will return without waiting for the data transfer
to finish. It is up to the user to be sure the data transfer has been done when
he actually wants to use the data. This can be checked by using the
WAIT\_FOR\_ASYNC\_QUEUE subroutine.

```
SUBROUTINE SUB(MYTEST)
LOGICAL, INTENT(IN) :: MYTEST
TYPE(FIELD_2D_OWNER) :: FW
TYPE(FIELD_2D_OWNER) :: FW2

CALL FW%INIT(/1,0/, /10,10/)
CALL FW2%INIT(/1,0/, /10,10/)

!Do stuff with FW on GPUs
!Then transfer data to CPU
CALL FW%SYNC_HOST_RDONLY(QUEUE=2)

!Do stuff with FW2 on GPUs
!We didn't have to wait for the data transfer of FW to finish

!Make sure the data transfer for FW is finished
CALL WAIT_FOR_ASYNC_QUEUE(QUEUE=2)

...

```

## Statistics

Each field API variable maintains statistics about the time it spend on data
transfer and the number of time it happened. You can access them through the
field ```FW%STATS```

For instance:
```
...
NUM_CPU_GPU_TR=FW%STATS%TRANSFER_CPU_TO_GPU
AVG=FW%STATS%TOTAL_TIME_TRANSFER_CPU_TO_GPU/FW%STATS%TRANSFER_CPU_TO_GPU
write(*,*)"Num transfer CPU->GPU", NUM_CPU_GPU_TR
write(*,*)"Total/Avg Time spend on transfer CPU->GPU", NUM_CPU_GPU_TR, "/" AVG, 
...
```

# Public API

For field api type:
```
SUBROUTINE INIT(SELF)
SUBROUTINE FINAL(SELF)
SUBROUTINE DELETE_DEVICE
FUNCTION GET_VIEW(SELF, BLOCK_INDEX, ZERO) RESULT(VIEW_PTR)
SUBROUTINE GET_DEVICE_DATA_RDONLY (SELF, PPTR, QUEUE)
SUBROUTINE GET_DEVICE_DATA_RDWR (SELF, PPTR, QUEUE)
SUBROUTINE GET_HOST_DATA_RDONLY (SELF, PPTR, QUEUE)
SUBROUTINE GET_HOST_DATA_RDWR (SELF, PPTR, QUEUE)
SUBROUTINE SYNC_HOST_RDWR (SELF, QUEUE)
SUBROUTINE SYNC_HOST_RDONLY (SELF, QUEUE)
SUBROUTINE SYNC_DEVICE_RDWR (SELF, QUEUE)
SUBROUTINE SYNC_DEVICE_RDONLY (SELF, QUEUE)
SUBROUTINE COPY_OBJECT (SELF, LDCREATED)
SUBROUTINE WIPE_OBJECT (SELF, LDDELETED)
```

Utils:

```
SUBROUTINE WAIT_FOR_ASYNC_QUEUE(QUEUE)
TYPE FIELD_*D_PTR
```

Stats:
```
INTEGER :: TRANSFER_CPU_TO_GPU 
INTEGER :: TRANSFER_GPU_TO_CPU
REAL :: TOTAL_TIME_TRANSFER_CPU_TO_GPU
REAL :: TOTAL_TIME_TRANSFER_GPU_TO_CPU
```



