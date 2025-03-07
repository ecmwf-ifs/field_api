# (C) Copyright 2022- ECMWF.
# (C) Copyright 2022- Meteo-France.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation
# nor does it submit to any jurisdiction.


__all__ = ['NVHPCOpenMPCUDA']

from offload_backends.nvhpc import NVHPCOpenMP

class NVHPCOpenMPCUDA(NVHPCOpenMP):
    """
    A class that defines the macros needed for GPU offload using Nvidia's
    OpenMP implementation and CUDA runtime API.
    """

    _kdir_map = {
        'H2D': 'cudaMemcpyHostToDevice',
        'D2H': 'cudaMemcpyDeviceToHost'
    }

    @classmethod
    def runtime_api_import(cls):
        """
        Runtime API import.
        """

        _import = [super().runtime_api_import(),]
        _import += ["USE CUDAFOR",]

        return _import

    @classmethod
    def stream_decl(cls, symbols):
        """
        Declare variables used to store a CUDA stream.
        """

        return f"INTEGER(KIND=CUDA_STREAM_KIND) :: {','.join(symbols)}"

    @classmethod
    def dev_malloc_intf(cls):
        """
        The ISO_C interface for a device memory allocation.
        """

        intf = """
  INTEGER FUNCTION CUDA_MALLOC (PTR,SIZ) BIND (C, NAME='cudaMalloc')
    IMPORT :: C_PTR, C_SIZE_T
    INTEGER (C_SIZE_T), VALUE, INTENT(IN) :: SIZ
    TYPE (C_PTR), INTENT(OUT) :: PTR
  END FUNCTION CUDA_MALLOC
  """

        return intf.split('\n')

    @classmethod
    def dev_free_intf(cls):
        """
        The ISO_C interface for freeing device memory.
        """

        intf = """
  INTEGER FUNCTION CUDA_FREE (PTR) BIND (C, NAME='cudaFree')
    IMPORT :: C_PTR
    TYPE (C_PTR), VALUE, INTENT(IN) :: PTR
  END FUNCTION CUDA_FREE
  """

        return intf.split('\n')

    @classmethod
    def runtime_error_return_type_decl(cls, symbols):
        """
        Declaration for the variable used to store the runtime API error status.
        """

        return f"INTEGER :: {','.join(symbols)}"

    @classmethod
    def dev_malloc(cls, ptr, size, return_val="ISTAT"):
        """
        Allocate memory on device.
        """

        return f"{return_val} = CUDA_MALLOC({ptr}, {size})"

    @classmethod
    def dev_free(cls, ptr, return_val="ISTAT"):
        """
        Free device memory.
        """

        return f"{return_val} = CUDA_FREE({ptr})"

    @classmethod
    def register_host(cls, ptr, size, flags, return_val="ISTAT"):
        """
        Page-lock host memory.
        """

        return f"{return_val} = CUDA_HOST_REGISTER({ptr}, {size}, {flags})"

    @classmethod
    def register_host_set_flags(cls, flag_var, val):
        """
        Set flags used to control page-locking of host memory.
        """

        return f"{flag_var} = {val} !... Corresponds to cudaHostRegisterMapped"

    @classmethod
    def register_host_decl_flags(cls, flag_var):
        """
        Declare variable used to store flags for controlling page-locking of host memory.
        """

        return f"INTEGER(C_INT) :: {flag_var}"

    @classmethod
    def unregister_host(cls, ptr, return_val="ISTAT"):
        """
        Unpin (i.e. undo page-locking) host memory.
        """

        return f"{return_val} = CUDA_HOST_UNREGISTER({ptr})"

    @classmethod
    def host_register_intf(cls):
        """
        The ISO_C interface for page-locking host memory.
        """

        intf = """
   INTEGER FUNCTION CUDA_HOST_REGISTER (PTR, SIZ, FLAGS) BIND (C, NAME='cudaHostRegister')
     IMPORT :: C_PTR, C_SIZE_T, C_INT
     TYPE (C_PTR), VALUE, INTENT(IN) :: PTR
     INTEGER (C_SIZE_T), VALUE, INTENT(IN) :: SIZ
     INTEGER (C_INT), VALUE, INTENT(IN) :: FLAGS
   END FUNCTION CUDA_HOST_REGISTER
  """

        return intf.split('\n')

    @classmethod
    def host_unregister_intf(cls):
        """
        The ISO_C interface for un-pinning (i.e. undo page-locking) host memory.
        """

        intf = """
   INTEGER FUNCTION CUDA_HOST_UNREGISTER (PTR) BIND (C, NAME='cudaHostUnregister')
     IMPORT :: C_PTR
     TYPE (C_PTR), VALUE, INTENT(IN) :: PTR
   END FUNCTION CUDA_HOST_UNREGISTER
  """

        return intf.split('\n')

    @classmethod
    def memcpy_2D(cls, src, src_pitch, dst, dst_pitch, width, height, kdir, return_val="ISTAT"):
        """
        Copy a strided memory region from source (src) to destination (dst).
        """

        kind = cls._kdir_map[kdir]
        return f"{return_val} = CUDA_MEMCPY_2D({dst}, {dst_pitch}, {src}, {src_pitch}, {width}, {height}, {kind})"

    @classmethod
    def memcpy_2D_async(cls, src, src_pitch, dst, dst_pitch, width, height, stream, kdir, return_val="ISTAT"):
        """
        Asynchronously copy a strided memory region from source (src) to destination (dst).
        """

        kind = cls._kdir_map[kdir]
        return f"{return_val} = CUDA_MEMCPY_2D_ASYNC({dst}, {dst_pitch}, {src}, {src_pitch}, {width}, {height}, {kind}, {stream})"

    @classmethod
    def memcpy_to_device_async(cls, dev, host, size, stream, return_val, **kwargs):
        """
        Asynchornously copy a contiguous section of data from host to device.
        """

        return f"{return_val} = CUDA_MEMCPY_ASYNC ({dev}, c_loc({host}), {size}, cudaMemcpyHostToDevice, {stream})"

    @classmethod
    def memcpy_from_device_async(cls, dev, host, size, stream, return_val, **kwargs):
        """
        Asynchronously copy a contiguous section of data from device to host.
        """

        return f"{return_val} = CUDA_MEMCPY_ASYNC (c_loc({host}), {dev}, {size}, cudaMemcpyDeviceToHost, {stream})"

    @classmethod
    def create_stream(cls, stream, return_val, **kwargs):
        """
        Create a CUDA stream.
        """

        return f"{return_val} = CUDASTREAMCREATE({stream})"

    @classmethod
    def async_wait(cls, stream, return_val):
        """
        Wait of all operations associated with a stream to complete.
        """

        return f"{return_val} = CUDASTREAMSYNCHRONIZE({stream})"

    @classmethod
    def memcpy_async_intf(cls):
        """
        The ISO_C interface for the runtime function that asynchornously copies a
        contiguous section of data from host to device.
        """

        intf = """
   INTEGER FUNCTION CUDA_MEMCPY_ASYNC (DST, SRC, COUNT, KIND, STREAM) BIND (C, NAME='cudaMemcpyAsync')
     IMPORT :: C_PTR, C_SIZE_T, C_INT, CUDA_STREAM_KIND
     TYPE (C_PTR), VALUE, INTENT(IN) :: DST
     TYPE (C_PTR), VALUE, INTENT(IN) :: SRC
     INTEGER (C_SIZE_T), VALUE, INTENT(IN) :: COUNT
     INTEGER (C_INT), VALUE, INTENT(IN) :: KIND
     INTEGER (KIND=CUDA_STREAM_KIND), VALUE, INTENT(IN) :: STREAM
   END FUNCTION CUDA_MEMCPY_ASYNC
  """

        return intf.split('\n')

    @classmethod
    def memcpy_2D_intf(cls):
        """
        The ISO_C interface for the runtime function to copy a discontiguous section of memory
        from host to device and vice versa.
        """

        intf = """
   INTEGER FUNCTION CUDA_MEMCPY_2D(DST, DSTPITCH, SRC, SRCPITCH, WIDTH, HEIGHT, KIND) BIND (C, NAME='cudaMemcpy2D')
     IMPORT :: C_PTR, C_SIZE_T, C_INT
     TYPE (C_PTR), VALUE, INTENT(IN) :: DST
     INTEGER (C_SIZE_T), VALUE, INTENT(IN) :: DSTPITCH
     TYPE (C_PTR), VALUE, INTENT(IN) :: SRC
     INTEGER (C_SIZE_T), VALUE, INTENT(IN) :: SRCPITCH
     INTEGER (C_SIZE_T), VALUE, INTENT(IN) :: WIDTH
     INTEGER (C_SIZE_T), VALUE, INTENT(IN) :: HEIGHT
     INTEGER (C_INT), VALUE, INTENT(IN) :: KIND
   END FUNCTION CUDA_MEMCPY_2D
   INTEGER FUNCTION CUDA_MEMCPY_2D_ASYNC(DST, DSTPITCH, SRC, SRCPITCH, WIDTH, HEIGHT, KIND, STREAM) BIND (C, NAME='cudaMemcpy2DAsync')
     IMPORT :: C_PTR, C_SIZE_T, C_INT, CUDA_STREAM_KIND
     TYPE (C_PTR), VALUE, INTENT(IN) :: DST
     INTEGER (C_SIZE_T), VALUE, INTENT(IN) :: DSTPITCH
     TYPE (C_PTR), VALUE, INTENT(IN) :: SRC
     INTEGER (C_SIZE_T), VALUE, INTENT(IN) :: SRCPITCH
     INTEGER (C_SIZE_T), VALUE, INTENT(IN) :: WIDTH
     INTEGER (C_SIZE_T), VALUE, INTENT(IN) :: HEIGHT
     INTEGER (C_INT), VALUE, INTENT(IN) :: KIND
     INTEGER (KIND=CUDA_STREAM_KIND), VALUE, INTENT(IN) :: STREAM
   END FUNCTION CUDA_MEMCPY_2D_ASYNC
  """

        return intf.split('\n')
