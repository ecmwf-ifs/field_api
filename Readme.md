# Field API

Field API aims to ease the management and the transfer of data between CPUs and
GPUs for the Météo-France/ECMWF software.

The API is using fypp heavily to generate the code for several types and
dimensions. It might look complicated, but if you are just using the API then
you should not worry about it.

# Compilation

## Requirements
Building FIELD_API requires:
- A Fortran 2008 compliant compiler with support for:
  - OpenMP for CPU multi-threading
  - OpenACC/OpenMP/CUDA for GPU offload (optional)
- CMake (>= 3.24)
- [ecbuild](https://github.com/ecmwf/ecbuild) (cloned if not found)
- [fypp](https://github.com/aradi/fypp) (cloned if not found)
- [fiat](https://github.com/ecmwf-ifs/fiat/) (optional)

To build FIELD_API without fiat, the path to the directory containing the utility modules `oml_mod.F90`, `abor1.F90` and `parkind1.F90` must be specified using the CMake variable `UTIL_MODULE_PATH`.

## Build and test
```
mkdir build
cd build
cmake .. # configure FIELD_API build
make # build FIELD_API
make install #optional, install FIELD_API
ctest #Optional, will run the tests
```

## Using FIELD_API
FIELD_API can be compiled as an external library and used in any CMake project simply by setting the environment variable `field_api_ROOT` to the FIELD_API builddir. Alternatively, FIELD_API can also be installed to a location on the `PATH`.

## Arch files
Architecture specific environment variables and compiler flags can be set by sourcing one of the `env.sh` files provided in the `arch` directory.

## Features
Features of FIELD_API can be toggled by passing the following argument to the CMake configure step: `-DENABLE_<FEATURE>=ON/OFF`. The table below lists all the available features:

| Feature | Default | Description |
|:--- |:--- |:--- |
| TESTS | ON | Build the testing suite. |
| BUDDY_MALLOC | ON | Enable the use of a binary buddy memory allocator for the shadow host allocation for `FIELD%DEVPTR`. This option is switched off if CUDA is enabled.|
| ACC | ON | Enable the use of OpenACC for GPU offload. Currently only supported on NVHPC. |
| OMP_OFFLOAD | OFF | Enable the use of OpenMP for GPU offload. Currently only supported on NVHPC. |
| SINGLE_PRECISION | ON | Enable the compilation of field_api in single precision |
| DOUBLE_PRECISION | ON | Enable the compilation of field_api in double precision |
| CUDA | OFF | Enable the use of CUDA for GPU offload. Disables the use of the buddy memory allocator, removes the shadow host allocation for `FIELD%DEVPTR` and allocates owned fields (see below) in pinned (page-locked) host memory.|
| FIELD_GANG | ON | Enable packed storage of groups of fields. This feature is not supported for the Cray compiler as it cannot resolve the underlying polymorphism.|

## Supported compilers
The library has been tested with the nvhpc toolkit from Nvidia, version 23.9
and is continually tested with newer releases. Please note that GPU offload is currently
only supported for Nvidia compilers. It has also been tested on CPU (-DENABLE_ACC=OFF)
with GCC 12, Intel 2021 and CCE17.

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
USE FIELD_MODULE
USE FIELD_FACTORY_MODULE
CLASS(FIELD_2RB), POINTER :: FO => NULL()
TYPE(FIELD_2D_VIEW_PTR) :: V

!Will create a field with the first dimension going from 1 to 10 and second from 1 to OMP_NUM_THREADS
CALL FIELD_NEW(FO, LBOUNDS=/1,1/, UBOUNDS=/10,1/)

DO IBLK=1,NBLKS
  V => FO%GET_VIEW(IBLK)
  !do stuff with v
ENDDO
  
CALL FIELD_DELETE(FO)
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
USE FIELD_MODULE
USE FIELD_FACTORY_MODULE
INTEGER, INTENT(INOUT) :: MYDATA(:,:)
CLASS(FIELD_2RB), POINTER :: FW => NULL()

!Wrap MYDATA into field wrapper FW
CALL FIELD_NEW(FW, DATA=MYDATA)

!do stuff

CALL FIELD_DELETE(FW)

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
USE FIELD_MODULE
USE FIELD_FACTORY_MODULE
CLASS(FIELD_2RB), POINTER :: FO => NULL()

!Allocate data with field API 
!The allocated data will have a first dimension
!going from 1 to 10 and a second from 0 to 10.
CALL FIELD_NEW(FO, /1,0/, /10,10/, PERSISTENT=.FALSE.)

!do stuff

CALL FIELD_DELETE(FO)
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
USE FIELD_MODULE
USE FIELD_FACTORY_MODULE
CLASS(FIELD_2RB), POINTER :: FO => NULL()

!Declare a field owner, no allocation will happen here
CALL FIELD_NEW(FO, /1,0/, /10,10/, PERSISTENT=.FALSE., DELAYED=.TRUE.)

IF (MYTEST) THEN
!do stuff with FO
!allocation wil happen here
ENDIF

CALL FIELD_DELETE(FO)
!The data will be freed if MYTEST was true, otherwise there are no data to deallocate
```

### Initialisation

In the case of field owner it is possible to initiliase it with a specific
value at creation time by adding the INIT\_VALUE optional argument.

```
   CLASS(FIELD_2IM), POINTER :: O => NULL()
   !This field owner will be initialised to 3
   CALL FIELD_NEW(O, LBOUNDS=[1,1], UBOUNDS=[10,10], INIT_VALUE=3_JPIM)
```

It is also possible to activate a debug value to initialise all non-initialised
owner. To do so it is necessary to import the module *field_init_debug_module*
and set *use_init_debug_value* to true. Then one can *set
init_debug_value_jpim* to a custom value.

```
   USE FIELD_INIT_DEBUG_VALUE_MODULE
   USE_INIT_DEBUG_VALUE = .TRUE.
   INIT_DEBUG_VALUE_JPIM = -7
   !This field owner will be initialised to -7
   CALL FIELD_NEW(O, LBOUNDS=[1,1], UBOUNDS=[10,10])
```

## Data Transfers
A field has ten type bound procedures that can be used to transfer data between the device and host.  Five `SYNC` methods that just copy the data using
the specified access mode, and five `GET` methods that also additionally return a pointer to access the data:
* ``SUBROUTINE SYNC_DEVICE_DATA_RDONLY (SELF)``
* ``SUBROUTINE SYNC_DEVICE_DATA_WRONLY (SELF)``
* ``SUBROUTINE SYNC_DEVICE_DATA_RDWR (SELF)``
* ``SUBROUTINE SYNC_HOST_DATA_RDONLY (SELF)``
* ``SUBROUTINE SYNC_HOST_DATA_RDWR (SELF)``

* ``SUBROUTINE GET_DEVICE_DATA_RDONLY (SELF, PPTR)``
* ``SUBROUTINE GET_DEVICE_DATA_WRONLY (SELF, PPTR)``
* ``SUBROUTINE GET_DEVICE_DATA_RDWR (SELF, PPTR)``
* ``SUBROUTINE GET_HOST_DATA_RDONLY (SELF, PPTR)``
* ``SUBROUTINE GET_HOST_DATA_RDWR (SELF, PPTR)``

Where``DEVICE/HOST`` indicates the transfer direction and ``RDONLY/RDWR/WRONLY`` indicates the access specifier, e.g. data copied to device using a `RDONLY`
access specifier will **not** be copied back to host if a subsequent ``RDWR`` host access is issued. The difference between the ``GET`` and ``SYNC`` method
is just their interface. The ``GET`` methods are called with a pointer argument, which must have the same rank as the field, that will be associated with
the transferred data at its destination. The ``SYNC`` method is called without any arguments and will only perform the data transfers and update the field's
inner pointers.

## Asynchronism

This functionnality is still being tested.

By default all data transfers are synchronous. So every call to the subroutines
GET\_HOST\_DATA, GET\_DEVICE\_DATA, SYNC\_HOST, SYNC\_DEVICE will stop the
program until the data are actually transfered. But sometimes it is possible to
interleave the data transfer with the computations. To do so you can add the
QUEUE parameter, which must have a value greater than or equal to 1, when calling
the aforementioned subroutines. With this QUEUE parameter the user will specify on
which queue he wants the data transfer to happen, and the subroutines will return
without waiting for the data transfer to finish. It is up to the user to be sure
the data transfer has been done when he actually wants to use the data. This can
be checked by using the WAIT\_FOR\_ASYNC\_QUEUE subroutine.

```
SUBROUTINE SUB(MYTEST)
USE FIELD_MODULE
USE FIELD_FACTORY_MODULE
CLASS(FIELD_2RB), POINTER :: FO => NULL()
CLASS(FIELD_2RB), POINTER :: FO2 => NULL()
LOGICAL, INTENT(IN) :: MYTEST

CALL FIELD_NEW(FO, /1,0/, /10,10/)
CALL FIELD_NEW(FO2, /1,0/, /10,10/)

!Do stuff with FO on GPUs
!Then transfer data to CPU
CALL FO%SYNC_HOST_RDONLY(QUEUE=2)

!Do stuff with FO2 on GPUs
!We didn't have to wait for the data transfer of FO to finish

!Make sure the data transfer for FO is finished
CALL WAIT_FOR_ASYNC_QUEUE(QUEUE=2)

...

```

NB: Asynchronous offload requires the CUDA backend, which can be
enabled by passing `-DENABLE_CUDA=ON` at build time.

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

## Note on GET\_VIEW

GET\_VIEW must only be called in sections of code running on the host. The
field's data must be present on the host. It will not work if the data are on
the device or if the field has not been allocated yet (when using the DELAY
option).

## Cloning fields with FIELD\_CLONE\_ON_

The subroutines FIELD_CLONE_ON_HOST and FIELD_CLONE_ON_DEVICE let a field be
cloned into a newly created FIELD_OWNER. The subroutines takes two arguments YL
and YR. YL is the field that will receive the copy and YR is the field to be
copied. YR is optional and can also be null, if any of those cases YL is set to
null and no cloning is done.

```
...
  USE FIELD_CLONE_MODULE, ONLY: FIELD_CLONE_ON_HOST
  CLASS(FIELD_1RB), POINTER :: MYCLONE => NULL()
...
  CALL FIELD_CLONE_ON_HOST(MYCLONE, FIELD_TO_BE_CLONED)
...
```

# Groups of Fields

FIELD_API provides two abstractions to represent packed (i.e. interleaved) storage of fields:
1. `FIELD_GANG`: packed storage of fields where the consituent fields are of the same shape and size.
2. `FIELD_STACK`: packed storage of fields where each the consituent fields are of arbitrary size
and have a shape of either `${RANK}$` or `${RANK-1}$`. It should be noted that the limitation
on shape here is purely due to the memory blocking in the IFS around which FIELD_API has been designed,
combined with the fact that discontiguous memory sections cannot be reshaped freely.

## `FIELD_GANG`

A `FIELD_GANG` can be created via a call to the `FIELD_NEW` constructor, just like any other wrapped or
owned field. The only extra argument required is an allocatable vector of type `FIELD_${RANK-1}${SUFF}$_PTR`,
which will contain pointers to the members, or children, of the `GANG`, e.g.:
```fortran
CLASS(FIELD_3RB), POINTER :: F_GANG
TYPE(FIELD_2RB_PTR), ALLOCATABLE :: GANG_CHLDREN(:)

CALL FIELD_NEW(F_GANG, CHILDREN=GANG_CHILDREN, ...)
```

Host/device pointers, as well as per-block view pointers on host, can be obtained for both the `GANG` and its
children via the usual API. It should be noted that to ensure data coherence, the whole `GANG` will move
together between host and device. An example of how the `FIELD_GANG` can be used is found in `tests/test_gang.F90`.

## `FIELD_STACK`

A `FIELD_STACK` is again created via the `FIELD_NEW` constructor by passing the `LSTACK=.TRUE.` argument.
Furthermore, the flexibility of the `FIELD_STACK` abstraction is exposed to the user via three further 
optional arguments:
1. `MEMBER_MAP`: a list of tuples representing the range of each member (i.e. child). This list
must therefore be of length `2*NMEMBERS`, where `NMEMBERS` is the number of members.
2. `MEMBER_LBOUNDS`: a list of length `NMEMBERS` containing lower bound overrides for the members.
3. `MEMBER_RANKS`: by default, a member defined such that `MEMBER_MAP(I) == MEMBER_MAP(I+1)` is
assumed to be of `${RANK-1}$`. This can be overriden by the user by explicitly providing the rank
for each member.

The constructor arguments for the `FIELD_STACK` are best illustrated with an example:
```fortran
CLASS(FIELD_3RB), POINTER :: F_STACK
CLASS(FIELD_2RB), POINTER :: F_1
CLASS(FIELD_3RB), POINTER :: F_2, F_3
INTEGER(KIND=JPIM) :: MEMBER_MAP(3) = (/1,1,2,4,5,8/)

CALL FIELD_NEW(F_STACK, LSTACK=.TRUE, MEMBER_MAP=MEMBER_MAP, ...)
! The remaining arguments are what one would expect for an owned or wrapped field

CALL GET_STACK_MEMBER(F_STACK, 1, F_1) ! 2D field representing one element of the 2nd dim of F_STACK
CALL GET_STACK_MEMBER(F_STACK, 2, F_2) ! 3D field representing three elements of the 2nd dim of F_STACK
CALL GET_STACK_MEMBER(F_STACK, 3, F_3) ! 3D field representing four elements of the 2nd dim of F_STACK
```

In the example above, all three member fields could have been forced to be 3D by passing the argument
`MEMBER_RANKS=[3,3,3]`. The keen reader may have also observed that members of the `FIELD_STACK` are
accessed via the `GET_STACK_MEMBER` method, availabe via the `FIELD_FACTORY` module. This is to shield
users from the nightmarish derived-type casting syntax in Fortran. Finally, just like the `FIELD_GANG`,
data movement between host and device on the `FIELD_STACK` always happens as a group. Further examples
of the `FIELD_STACK` can be found in `tests/test_field_stack_*.F90`.

# Public API

For field api type:
```
SUBROUTINE FIELD_NEW(SELF, ...)
SUBROUTINE FIELD_RESIZE(SELF, ...)
SUBROUTINE FIELD_DELETE(SELF)
SUBROUTINE DELETE_DEVICE
FUNCTION GET_VIEW(SELF, BLOCK_INDEX, ZERO) RESULT(VIEW_PTR)
SUBROUTINE GET_DEVICE_DATA_RDONLY (SELF, PPTR, QUEUE)
SUBROUTINE GET_DEVICE_DATA_RDWR (SELF, PPTR, QUEUE)
SUBROUTINE GET_DEVICE_DATA_WRONLY (SELF, PPTR, QUEUE)
SUBROUTINE GET_HOST_DATA_RDONLY (SELF, PPTR, QUEUE)
SUBROUTINE GET_HOST_DATA_RDWR (SELF, PPTR, QUEUE)
SUBROUTINE SYNC_HOST_RDWR (SELF, QUEUE)
SUBROUTINE SYNC_HOST_RDONLY (SELF, QUEUE)
SUBROUTINE SYNC_DEVICE_RDWR (SELF, QUEUE)
SUBROUTINE SYNC_DEVICE_RDONLY (SELF, QUEUE)
SUBROUTINE SYNC_DEVICE_WRONLY (SELF, QUEUE)
SUBROUTINE COPY_OBJECT (SELF, LDCREATED)
SUBROUTINE WIPE_OBJECT (SELF, LDDELETED)
SUBROUTINE GET_DIMS (SELF, LBOUNDS, UBOUNDS)
```

Utils:

```
SUBROUTINE WAIT_FOR_ASYNC_QUEUE(QUEUE)
TYPE FIELD_*D_PTR
SUBROUTINE FIELD_CLONE_ON_HOST(YL, YR)
SUBROUTINE FIELD_CLONE_ON_DEVICE(YL, YR)
```

Stats:
```
INTEGER :: TRANSFER_CPU_TO_GPU 
INTEGER :: TRANSFER_GPU_TO_CPU
REAL :: TOTAL_TIME_TRANSFER_CPU_TO_GPU
REAL :: TOTAL_TIME_TRANSFER_GPU_TO_CPU
```

# License

The field API library is licenced under the Apache licence, version 2.0.

[buddy_alloc](https://github.com/spaskalev/buddy_alloc) is property of Stanislav Paskalev and licensed under the BSD Zero Clause License

# Contributing

Contributions to field API are welcome. 
In order to do so, please open an issue where a feature request or bug can be discussed. 
Then create a pull request with your contribution and sign the [contributors license agreement (CLA)](https://bol-claassistant.ecmwf.int/ecmwf-ifs/field_api).
