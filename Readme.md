# Field API

Field API aims to ease the management and the transfer of data between CPUs and
GPUs for the MF/ECMWF software.

The API is using fypp heavily to generate the code for several type and
dimension. It might looks complicated, but if you are just using the API then
you should not worry about it.

This library should be consired highly experimental.

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
SUBROUTINE SUB1()
TYPE(FIELD_2D_OWNER) :: FW
TYPE(FIELD_2D_VIEW_PTR) :: V

!Will create a field with the first dimension going from 1 to 10 and second from 1 to OMP_NUM_THREADS
FW%INIT(/1.1/, /10,1/)

DO JLON=KIDIA,KFDIA
  V => FW%GET_VIEW(JLON)
  !do stuff with v
ENDDO
  
FW%FINALIZE()
```

Furthermore field API uses provides two way of encapsulating the data: wrappers
and owners

## WRAPPER

The field API wrappers (eg. FIELD\_2D\_WRAPPER) provide a way to encapsulate
data which was already allocated before entering a part of the code which use
field API. It is really just adding a wrapper around an array.

For instance, let say there are some data used in the code that has nothing to
dowith GPUs. The data are allocated there and used there. But at some point,
maybe deeper in the code, those are data are in fact needed on GPU. Instead of
having to declare a field API object high in the call stack, one could simply
declare a field api wrapper when needed.

```
SUBROUTINE SUB(D)
INTEGER, INTENT(INOUT) :: MYDATA(:,:)
TYPE(FIELD_2D_WRAPPER) :: FW

!Wrap MYDATA into field wrapper FW
FW%INIT(MYDATA)

!do stuff

FW%FINALIZE()

!MYDATA is still accessible
MYDATA(1,2) = 7
```

## OWNER

The field API owners (eg. FIELD\_2D\_OWNER) provide a way to declare and
allocate  data to be used on CPU and GPU. The data is allocated by the API, the
user don't have to allocate data by itself. Simillarly, the user don't
deallocate the data by himself, it is done by the API. When create a owner, the
user will need to provide the two arrays used to specify the lower and upper
bounds of the array that will be created by field api.

```
SUBROUTINE SUB()
TYPE(FIELD_2D_OWNER) :: FW

!Allocate data with field API 
!The allocated data will have a first dimension
!going from 1 to 10 and a second from 0 to 10.
FW%INIT(/1.0/, /10,10/, PERSISTENT=.FALSE.)

!do stuff

FW%FINALIZE()
!The data has now be freed on CPU and GPU and cannot be accessed anymore
```

### Delayed allocations

Field owners also provide a way to delay the allocation of data. The user can
ask a field owner to be created without allocating the data. The allocation
would then happened only if the data would be requested at some point, later in
the program. It can be useful one don't want to waste memory on data that might
be only conditionally used. But please keep in mind, that allocating data can
be slow and will slow down the program if done during a compuation heavy part
of the code.

```
SUBROUTINE SUB(MYTEST)
LOGICAL, INTENT(IN) :: MYTEST
TYPE(FIELD_2D_OWNER) :: FW

!Declare a field owner, no allocation will happen here
FW%INIT(/1.0/, /10,10/, PERSISTENT=.FALSE., DELAYED=.TRUE.)

IF (MYTEST) THEN
!do stuff with FW
!allocation wil happen here
ENDIF

FW%FINALIZE()
!The data will be freed if MYTEST was true, otherwise there are no data to deallocate
```

## Asynchronism

## Statistics

# API listing

TODO
