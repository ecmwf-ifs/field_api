# (C) Copyright 2022- ECMWF.
# (C) Copyright 2022- Meteo-France.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation
# nor does it submit to any jurisdiction.


__all__ = ['NVHPCOpenACC']

class NVHPCOpenACC():
    """
    A class that defines the macros needed for GPU offload using Nvidia's
    OpenACC implementation.
    """

    pragma = '!$acc'
    _data_attributes = ['private', 'copy', 'copyout', 'copyin', 'present', 'deviceptr', 'create']
    _loop_attributes = ['gang', 'vector', 'worker', 'seq']
    _declare_attributes = ['create', 'device_resident', 'deviceptr']

    @classmethod
    def runtime_api_import(cls):
        """
        Runtime API import.
        """

        return "USE OPENACC"

    @classmethod
    def c_devptr_declaration(cls, symbols):
        """
        Type declaration for a `C_PTR` on device.
        """

        return f"TYPE(C_DEVPTR) :: {','.join(symbols)}"

    @classmethod
    def host_data_start(cls, symbols):
        """
        Pragma to mark the start of a `host_data` region.
        """

        return f"!$acc host_data use_device({','.join(symbols)})"

    @classmethod
    def host_data_end(cls):
        """
        Pragma to mark the end of a `host_data` region.
        """

        return "!$acc end host_data"

    @classmethod
    def devptr_c_loc(cls, symbol):
        """
        Function to determine the C address of a device variable.
        """

        return f"C_DEVLOC({symbol})"

    @classmethod
    def copy_to_device_1D(cls, dev, host, size):
        """
        Copy a contiguous section of data from host to device.
        """

        return f"CALL ACC_MEMCPY_TO_DEVICE ({dev}, {host}, {size})"

    @classmethod
    def copy_to_device_1D_async(cls, dev, host, queue, size):
        """
        Asynchornously copy a contiguous section of data from host to device.
        """

        return f"CALL ACC_MEMCPY_TO_DEVICE_ASYNC ({dev}, {host}, {size}, {queue})"

    @classmethod
    def copy_from_device_1D(cls, dev, host, size):
        """
        Copy a contiguous section of data from device to host.
        """

        return f"CALL ACC_MEMCPY_FROM_DEVICE ({host}, {dev}, {size})"

    @classmethod
    def copy_from_device_1D_async(cls, dev, host, size, queue):
        """
        Asynchronously copy a contiguous section of data from device to host.
        """

        return f"CALL ACC_MEMCPY_FROM_DEVICE_ASYNC ({host}, {dev}, {size}, {queue})"

    @classmethod
    def host_mapped_dev_alloc(cls, data):
        """
        Allocate host-mapped memory on device.
        """

        return f"!$acc enter data create ({','.join(data)})"

    @classmethod
    def host_mapped_dev_free(cls, data):
        """
        Free host-mapped memory on device.
        """

        return f"!$acc exit data delete ({','.join(data)})"

    @classmethod
    def attach_dev_ptr(cls, ptr):
        """
        Attach device pointer to its target on device.
        """

        return f"!$acc enter data attach ({ptr})"

    @classmethod
    def detach_dev_ptr(cls, ptr):
        """
        Detach device pointer from its target on device.
        """

        return f"!$acc exit data detach ({ptr})"

    @classmethod
    def launch_kernel(cls, **kwargs):
        """
        Launch an implicitly mapped parallel kernel on device.
        """

        _data_spec = ""
        for attr in cls._data_attributes:
            decl = kwargs.get(attr, None)
            if decl:
                _data_spec += f"{attr}({','.join(decl)}) "

        return f"!$acc kernels {_data_spec}"

    @classmethod
    def end_kernel(cls):
        """
        End an implicitly mapped parallel kernel on device.
        """

        return "!$acc end kernels"

    @classmethod
    def async_wait(cls, stream):
        """
        Wait for the operations queued on a stream to complete.
        """

        return f"!$acc wait ({stream})"

    @classmethod
    def launch_parallel_loop(cls, **kwargs):
        """
        Launch an explicitly mapped parallel kernel on device.
        """

        _loop_spec = ""
        for attr in cls._loop_attributes:
            if kwargs.get(attr, None):
                _loop_spec += f"{attr} "

        for attr in cls._data_attributes:
            decl = kwargs.get(attr, None)
            if decl:
                _loop_spec += f"{attr}({','.join(decl)}) "

        return f"!$acc parallel loop {_loop_spec}"

    @classmethod
    def end_parallel_loop(cls):
        """
        End an explicitly mapped parallel kernel on device.
        """

        return "!$acc end parallel loop"

    @classmethod
    def annotate_parallel_loop(cls, **kwargs):
        """
        Annotate a loop in a device parallel region.
        """

        _loop_spec = ""
        for attr in cls._loop_attributes:
            if kwargs.get(attr, None):
                _loop_spec += f"{attr} "

        for attr in cls._data_attributes:
            decl = kwargs.get(attr, None)
            if decl:
                _loop_spec += f"{attr}({','.join(decl)}) "

        return f"!$acc loop {_loop_spec}"

    @classmethod
    def declare(cls, **kwargs):
        """
        Issue a device declaration for a host-mapped symbol.
        """

        _decl_spec = ""
        for attr in cls._declare_attributes:
            decl = kwargs.get(attr, None)
            if decl:
                _decl_spec += f"{attr}({','.join(decl)}) "

        return f"!$acc declare {_decl_spec}"

    @classmethod
    def launch_serial_kernel(cls, **kwargs):
        """
        Launch a serial kernel on device.
        """

        _data_spec = ""
        for attr in cls._data_attributes:
            decl = kwargs.get(attr, None)
            if decl:
                _data_spec += f"{attr}({','.join(decl)}) "

        return f"!$acc serial {_data_spec}"

    @classmethod
    def end_serial_kernel(cls):
        """
        End a serial device kernel.
        """

        return "!$acc end serial"

    @classmethod
    def update_device(cls, data):
        """
        Update host-mapped symbol on device.
        """

        return f"!$acc update device ({','.join(data)})"

    @classmethod
    def update_host(cls, data):
        """
        Update device-mapped symbol on host.
        """

        return f"!$acc update self ({','.join(data)})"
