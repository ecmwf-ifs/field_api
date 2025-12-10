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
    _data_attributes = ['private', 'copy', 'copyout', 'copyin', 'present', 'create']

    @classmethod
    def runtime_api_import(cls):
        """
        Runtime API import.
        """

        return "USE OPENACC"

    @classmethod
    def c_devptr_decl(cls, symbols):
        """
        Type declaration for a `C_PTR` on device.
        """

        return f"TYPE(C_DEVPTR) :: {','.join(symbols)}"

    @classmethod
    def host_data(cls, symbols):
        """
        Pragma to mark the start of a `host_data` region.
        """

        return f"!$acc host_data use_device({','.join(symbols)})"

    @classmethod
    def end_host_data(cls):
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
    def memcpy_to_device(cls, dev, host, size, **kwargs):
        """
        Copy a contiguous section of data from host to device.
        """

        return f"CALL ACC_MEMCPY_TO_DEVICE ({dev}, {host}, {size})"

    @classmethod
    def memcpy_to_device_async(cls, dev, host, size, **kwargs):
        """
        Copy a contiguous section of data from host to device.
        In the absence of the CUDA backend, asynchronous copies
        fallback to synchornous execution.
        """

        return f"CALL ACC_MEMCPY_TO_DEVICE ({dev}, {host}, {size})"

    @classmethod
    def memcpy_from_device(cls, dev, host, size, **kwargs):
        """
        Copy a contiguous section of data from device to host.
        """

        return f"CALL ACC_MEMCPY_FROM_DEVICE ({host}, {dev}, {size})"

    @classmethod
    def memcpy_from_device_async(cls, dev, host, size, **kwargs):
        """
        Copy a contiguous section of data from device to host.
        In the absence of the CUDA backend, asynchronous copies
        fallback to synchornous execution.
        """

        return f"CALL ACC_MEMCPY_FROM_DEVICE ({host}, {dev}, {size})"

    @classmethod
    def create(cls, symbols):
        """
        Allocate host-mapped memory on device.
        """

        return f"!$acc enter data create ({','.join(symbols)})"

    @classmethod
    def delete(cls, symbols):
        """
        Free host-mapped memory on device.
        """

        return f"!$acc exit data delete ({','.join(symbols)})"

    @classmethod
    def attach(cls, ptr):
        """
        Attach device pointer to its target on device.
        """

        return f"!$acc enter data attach ({','.join(ptr)})"

    @classmethod
    def detach(cls, ptr):
        """
        Detach device pointer from its target on device.
        """

        return f"!$acc exit data detach ({','.join(ptr)})"

    @classmethod
    def parallel_gang_vector_loop(cls, **kwargs):
        """
        Annotate a parallel gang vector loop.
        """

        _loop_spec = ""
        collapse = kwargs.get('collapse', None)
        if collapse:
            _loop_spec += f"collapse ({collapse}) "

        for attr in cls._data_attributes:
            decl = kwargs.get(attr, None)
            if decl:
                _loop_spec += f"{attr}({','.join(decl)}) "

        return f"!$acc parallel loop gang vector {_loop_spec}"

    @classmethod
    def parallel_gang_loop(cls, **kwargs):
        """
        Annotate a parallel gang loop.
        """

        _data_spec = ""
        for attr in cls._data_attributes:
            decl = kwargs.get(attr, None)
            if decl:
                _data_spec += f"{attr}({','.join(decl)}) "

        return f"!$acc parallel loop gang {_data_spec}"

    @classmethod
    def end_parallel_loop(cls):
        """
        End an explicitly mapped parallel kernel on device.
        """

        return "!$acc end parallel loop"

    @classmethod
    def parallel_vector_loop(cls, **kwargs):
        """
        Annotate a vector loop in a device parallel region.
        """

        _data_spec = ""
        for attr in cls._data_attributes:
            decl = kwargs.get(attr, None)
            if decl:
                _data_spec += f"{attr}({','.join(decl)}) "

        return f"!$acc loop vector {_data_spec}"

    @classmethod
    def declare(cls, symbols):
        """
        Issue a device declaration for a host-mapped symbol.
        """

        return f"!$acc declare create({','.join(symbols)})"

    @classmethod
    def serial(cls, **kwargs):
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
    def end_serial(cls):
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

    @classmethod
    def data(cls, **kwargs):
        """
        Pragma to mark the start of a `data` region.
        """

        _data_spec = ""
        for attr in cls._data_attributes:
            decl = kwargs.get(attr, None)
            if decl:
                _data_spec += f"{attr}({','.join(decl)}) "

        return f"!$acc data {_data_spec}"

    @classmethod
    def data_deviceptr(cls, symbols):
        """
        Pragma to mark the start of a `data deviceptr` region.
        """

        return f"!$acc data deviceptr({','.join(symbols)})"

    @classmethod
    def end_data(cls):
        """
        Pragma to mark the end of a `data` region.
        """

        return "!$acc end data"

    @classmethod
    def end_data_deviceptr(cls):
        """
        Pragma to mark the end of a `data deviceptr` region.
        """


        return "!$acc end data"

    @classmethod
    def reinit_gpu_context(cls):
        """
        Used to force reinitialization of GPU device.
        Usefull when not calling GPU transfer function from OpenMP master thread
        To use it you must have called the method *runtime_api_import* before
        """

        return "CALL ACC_SET_DEVICE_NUM(ACC_GET_DEVICE_NUM(ACC_DEVICE_NVIDIA), ACC_DEVICE_NVIDIA)"

    @classmethod
    def map_device_addr_intf(cls):
        """
        The ISO_C interface for `acc_map_data` that maps a given device address to a given host address.
        """

        intf = """
  SUBROUTINE ACC_MAP_DATA (HST_PTR, DEV_PTR, SIZ) BIND (C, NAME='acc_map_data')
    IMPORT :: C_PTR, C_SIZE_T
    TYPE (C_PTR), INTENT(IN) :: HST_PTR
    TYPE (C_PTR), INTENT(IN) :: DEV_PTR
    INTEGER (C_SIZE_T), VALUE, INTENT(IN) :: SIZ
  END SUBROUTINE ACC_MAP_DATA
  """
        return intf.split('\n')

    @classmethod
    def unmap_device_addr_intf(cls):
        """
        The ISO_C interface for unmapping device memory associated to a given host address.
        """

        intf = """
  SUBROUTINE ACC_UNMAP_DATA (HST_PTR) BIND (C, NAME='acc_unmap_data')
    IMPORT :: C_PTR
    TYPE (C_PTR), INTENT(IN) :: HST_PTR
  END SUBROUTINE ACC_UNMAP_DATA
  """
        return intf.split('\n')

    @classmethod
    def map_device_addr(cls, hst_ptr, dev_ptr, siz, offset, dev_id, return_val):
        """
        Map device address to host address.
        """

        return f"CALL ACC_MAP_DATA({hst_ptr}, {dev_ptr}, {siz})"

    @classmethod
    def unmap_device_addr(cls, hst_ptr, dev_id, return_val):
        """
        Unmap device memory associated to a given host address.
        """

        return f"CALL ACC_UNMAP_DATA({hst_ptr})"
