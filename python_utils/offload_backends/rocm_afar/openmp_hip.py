# (C) Copyright 2022- ECMWF.
# (C) Copyright 2022- Meteo-France.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation
# nor does it submit to any jurisdiction.


__all__ = ['ROCMAFAROpenMPHIP']

from offload_backends.rocm_afar import ROCMAFAROpenMP

class ROCMAFAROpenMPHIP(ROCMAFAROpenMP):
    """
    A class that defines the macros needed for GPU offload using AMD's
    flang based OpenMP implementation and HIP runtime API.
    """

    _kdir_map = {
        'H2D': '1',
        'D2H': '2'
    }

    @classmethod
    def runtime_api_import(cls):
        """
        Runtime API import.
        """

        _import = [super().runtime_api_import(),]
        _import += ["USE HIPFORT",]

        return _import

    @classmethod
    def stream_decl(cls, symbols):
        """
        Declare variables used to store a HIP stream.
        """

        return f"TYPE(C_PTR) :: {','.join(symbols)}"

    @classmethod
    def runtime_error_return_type_decl(cls, symbols):
        """
        Declaration for the variable used to store the runtime API error status.
        """

        return f"INTEGER(KIND(HIPSUCCESS)) :: {','.join(symbols)}"

    @classmethod
    def dev_malloc(cls, ptr, size, return_val="ISTAT"):
        """
        Allocate memory on device.
        """

        return f"{return_val} = HIPMALLOC({ptr}, {size})"

    @classmethod
    def dev_free(cls, ptr, return_val="ISTAT"):
        """
        Free device memory.
        """

        return f"{return_val} = HIPFREE({ptr})"

    @classmethod
    def register_host(cls, ptr, size, flags, return_val="ISTAT"):
        """
        Page-lock host memory.
        """

        return f"{return_val} = HIPHOSTREGISTER({ptr}, {size}, {flags})"

    @classmethod
    def register_host_set_flags(cls, flag_var):
        """
        Set flags used to control page-locking of host memory.
        """

        return f"{flag_var} = 0 !... Corresponds to hiphostregistermapped"

    @classmethod
    def register_host_decl_flags(cls, flag_var):
        """
        Declare variable used to store flags for controlling page-locking of host memory.
        """

        return f"INTEGER(KIND=4) :: {flag_var}"

    @classmethod
    def unregister_host(cls, ptr, return_val="ISTAT"):
        """
        Unpin (i.e. undo page-locking) host memory.
        """

        return f"{return_val} = HIPHOSTUNREGISTER({ptr})"

    @classmethod
    def memcpy_2D(cls, src, src_pitch, dst, dst_pitch, width, height, kdir, return_val="ISTAT"):
        """
        Copy a strided memory region from source (src) to destination (dst).
        """

        kind = cls._kdir_map[kdir]
        return f"{return_val} = HIPMEMCPY2D({dst}, {dst_pitch}, {src}, {src_pitch}, {width}, {height}, {kind})"

    @classmethod
    def memcpy_2D_async(cls, src, src_pitch, dst, dst_pitch, width, height, stream, kdir, return_val="ISTAT"):
        """
        Asynchronously copy a strided memory region from source (src) to destination (dst).
        """

        kind = cls._kdir_map[kdir]
        return f"{return_val} = HIPMEMCPY2DASYNC({dst}, {dst_pitch}, {src}, {src_pitch}, {width}, {height}, {kind}, {stream})"

    @classmethod
    def memcpy_to_device_async(cls, dev, host, size, stream, return_val, **kwargs):
        """
        Asynchornously copy a contiguous section of data from host to device.
        """

        return f"{return_val} = HIPMEMCPYASYNC ({dev}, c_loc({host}), {size}, 1, {stream})"

    @classmethod
    def memcpy_from_device_async(cls, dev, host, size, stream, return_val, **kwargs):
        """
        Asynchronously copy a contiguous section of data from device to host.
        """

        return f"{return_val} = HIPMEMCPYASYNC (c_loc({host}), {dev}, {size}, 2, {stream})"

    @classmethod
    def create_stream(cls, stream, return_val, **kwargs):
        """
        Create a CUDA stream.
        """

        return f"{return_val} = HIPSTREAMCREATE({stream})"

    @classmethod
    def async_wait(cls, stream, return_val):
        """
        Wait of all operations associated with a stream to complete.
        """

        return f"{return_val} = HIPSTREAMSYNCHRONIZE({stream})"
