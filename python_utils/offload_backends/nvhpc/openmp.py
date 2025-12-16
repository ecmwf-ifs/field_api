# (C) Copyright 2022- ECMWF.
# (C) Copyright 2022- Meteo-France.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation
# nor does it submit to any jurisdiction.


__all__ = ['NVHPCOpenMP']

class NVHPCOpenMP():
    """
    A class that defines the macros needed for GPU offload using Nvidia's
    OpenMP implementation.
    """

    pragma = '!$omp'
    _data_attributes = ['private', 'copy', 'copyout', 'copyin', 'present', 'create']

    _acc_omp_map = {
        'private': 'private',
        'copy': 'map_tofrom',
        'copyout': 'map_from',
        'copyin': 'map_to',
        'create': 'map_alloc',
        'present': 'map_to'
    }

    @classmethod
    def runtime_api_import(cls):
        """
        Runtime API import.
        """

        return "USE OMP_LIB"

    @classmethod
    def c_devptr_decl(cls, symbols):
        """
        Type declaration for a `C_PTR` on device.
        """

        return f"TYPE(C_PTR) :: {','.join(symbols)}"

    @classmethod
    def host_data(cls, symbols):
        """
        Pragma to mark the start of a `target data use_device_ptr` region.
        """

        return f"!$omp target data use_device_ptr({','.join(symbols)})"

    @classmethod
    def end_host_data(cls):
        """
        Pragma to mark the end of a `target data` region.
        """

        return "!$omp end target data"

    @classmethod
    def devptr_c_loc(cls, symbol):
        """
        Function to determine the C address of a device variable.
        """

        return f"C_LOC({symbol})"

    @classmethod
    def get_device_id(cls, dev_id):
        """
        Get device number corresponding to GPU.
        """

        return f"{dev_id} = omp_get_default_device()"

    @classmethod
    def get_device_id_import(cls):
        """
        Runtime API import for retrieving device ID.
        """

        return "USE OMP_LIB, ONLY: OMP_GET_DEFAULT_DEVICE"

    @classmethod
    def get_host_id(cls, hst_id):
        """
        Get device number corresponding to CPU.
        """

        return f"{hst_id} = omp_get_initial_device()"

    @classmethod
    def memcpy_to_device(cls, dev, host, size, offset, dev_id, hst_id, return_val):
        """
        Copy a contiguous section of data from host to device.
        """

        _str = f"{return_val} = OMP_TARGET_MEMCPY({dev}, c_loc({host}), {size}, "
        _str += f"{offset}, {offset}, {dev_id}, {hst_id})"
        return _str

    @classmethod
    def memcpy_to_device_async(cls, dev, host, size, offset, dev_id, hst_id, return_val, **kwargs):
        """
        Copy a contiguous section of data from host to device.
        In the absence of the CUDA backend, asynchronous copies
        fallback to synchornous execution.
        """

        _str = f"{return_val} = OMP_TARGET_MEMCPY({dev}, c_loc({host}), {size}, "
        _str += f"{offset}, {offset}, {dev_id}, {hst_id})"
        return _str

    @classmethod
    def memcpy_from_device(cls, dev, host, size, offset, dev_id, hst_id, return_val):
        """
        Copy a contiguous section of data from device to host.
        """

        _str = f"{return_val} = OMP_TARGET_MEMCPY(c_loc({host}), {dev}, {size}, "
        _str += f"{offset}, {offset}, {hst_id}, {dev_id})"
        return _str

    @classmethod
    def memcpy_from_device_async(cls, dev, host, size, offset, dev_id, hst_id, return_val, **kwargs):
        """
        Copy a contiguous section of data from device to host.
        In the absence of the CUDA backend, asynchronous copies
        fallback to synchornous execution.
        """

        _str = f"{return_val} = OMP_TARGET_MEMCPY(c_loc({host}), {dev}, {size}, "
        _str += f"{offset}, {offset}, {hst_id}, {dev_id})"
        return _str

    @classmethod
    def create(cls, symbols):
        """
        Allocate host-mapped memory on device.
        """

        return f"!$omp target enter data map(alloc:{','.join(symbols)})"

    @classmethod
    def delete(cls, symbols):
        """
        Free host-mapped memory on device.
        """

        return f"!$omp target exit data map(delete:{','.join(symbols)})"

    @classmethod
    def attach(cls, ptr):
        """
        Attach device pointer to its target on device.
        """

        return f"!$omp target enter data map(to:{','.join(ptr)})"

    @classmethod
    def detach(cls, ptr):
        """
        Detach device pointer from its target on device.
        """

        return f"!$omp target exit data map(release:{','.join(ptr)})"

    @classmethod
    def parallel_gang_vector_loop(cls, **kwargs):
        """
        Annotate a parallel gang vector loop.
        """

        _loop_spec = ""
        collapse = kwargs.get('collapse', None)
        if collapse:
            _loop_spec += f"collapse ({collapse})"

        for _attr in cls._data_attributes:
            attr = cls._acc_omp_map.get(_attr, None)
            decl = kwargs.get(_attr, None)
            if decl and attr:
                if 'map' in attr:
                    map_type = attr.split('_')[-1]
                    _loop_spec += f"map({map_type}:{','.join(decl)}) "
                else:
                    _loop_spec += f"{attr}({','.join(decl)}) "

        return f"!$omp target teams distribute parallel do {_loop_spec}"

    @classmethod
    def parallel_gang_loop(cls, **kwargs):
        """
        Annotate a parallel gang loop.
        """

        _data_spec = ""
        for _attr in cls._data_attributes:
            attr = cls._acc_omp_map.get(_attr, None)
            decl = kwargs.get(_attr, None)
            if decl and attr:
                if 'map' in attr:
                    map_type = attr.split('_')[-1]
                    _data_spec += f"map({map_type}:{','.join(decl)}) "
                else:
                    _data_spec += f"{attr}({','.join(decl)}) "

        return f"!$omp target teams distribute {_data_spec}"

    @classmethod
    def parallel_vector_loop(cls, **kwargs):
        """
        Annotate a vector loop in a device parallel region.
        """

        _data_spec = ""
        for _attr in cls._data_attributes:
            attr = cls._acc_omp_map.get(_attr, None)
            decl = kwargs.get(_attr, None)
            if decl and attr:
                if 'map' in attr:
                    map_type = attr.split('_')[-1]
                    _data_spec += f"map({map_type}:{','.join(decl)}) "
                else:
                    _data_spec += f"{attr}({','.join(decl)}) "

        return f"!$omp parallel do {_data_spec}"

    @classmethod
    def declare(cls, symbols):
        """
        Issue a device declaration for a host-mapped symbol.
        """

        return f"!$omp declare target({','.join(symbols)})"

    @classmethod
    def serial(cls, **kwargs):
        """
        Launch a serial kernel on device.
        """

        _data_spec = ""
        for _attr in cls._data_attributes:
            attr = cls._acc_omp_map.get(_attr, None)
            decl = kwargs.get(_attr, None)
            if decl and attr:
                if 'map' in attr:
                    map_type = attr.split('_')[-1]
                    _data_spec += f"map({map_type}:{','.join(decl)}) "
                else:
                    _data_spec += f"{attr}({','.join(decl)}) "

        return f"!$omp target teams distribute parallel do num_teams(1) num_threads(1) {_data_spec}"

    @classmethod
    def end_serial(cls):
        """
        End a serial device kernel.
        """

        return "!$omp end target"

    @classmethod
    def update_device(cls, data):
        """
        Update host-mapped symbol on device.
        """

        return f"!$omp target update to ({','.join(data)})"

    @classmethod
    def update_host(cls, data):
        """
        Update device-mapped symbol on host.
        """

        return f"!$omp target update from ({','.join(data)})"

    @classmethod
    def reinit_gpu_context(cls):
        """
        Used to force reinitialization of GPU device.
        Usefull when not calling GPU transfer function from OpenMP master thread
        To use it you must have called the method *runtime_api_import* before
        """

        return "CALL OMP_SET_DEFAULT_DEVICE(OMP_GET_DEFAULT_DEVICE())"

    @classmethod
    def map_device_addr_intf(cls):
        """
        The ISO_C interface for `omp_target_associate_ptr` that maps a given device address to a given host address.
        """

        intf = """
  INTEGER(C_INT) FUNCTION OMP_TARGET_ASSOCIATE_PTR (HST_PTR, DEV_PTR, SIZ, OFFSET, DEV_ID) BIND (C, NAME='omp_target_associate_ptr')
    IMPORT :: C_PTR, C_SIZE_T, C_INT
    TYPE (C_PTR), VALUE, INTENT(IN) :: HST_PTR
    TYPE (C_PTR), VALUE, INTENT(IN) :: DEV_PTR
    INTEGER (C_SIZE_T), VALUE, INTENT(IN) :: SIZ
    INTEGER (C_SIZE_T), VALUE, INTENT(IN) :: OFFSET
    INTEGER (C_INT), VALUE, INTENT(IN) :: DEV_ID
  END FUNCTION OMP_TARGET_ASSOCIATE_PTR
  """
        return intf.split('\n')

    @classmethod
    def unmap_device_addr_intf(cls):
        """
        The ISO_C interface for unmapping device memory associated to a given host address.
        """

        intf = """
  INTEGER(C_INT) FUNCTION OMP_TARGET_DISASSOCIATE_PTR (HST_PTR, DEV_ID) BIND (C, NAME='omp_target_disassociate_ptr')
    IMPORT :: C_PTR, C_INT
    TYPE (C_PTR), VALUE, INTENT(IN) :: HST_PTR
    INTEGER (C_INT), VALUE, INTENT(IN) :: DEV_ID
  END FUNCTION OMP_TARGET_DISASSOCIATE_PTR
  """
        return intf.split('\n')

    @classmethod
    def map_device_addr(cls, hst_ptr, dev_ptr, siz, offset, dev_id, return_val):
        """
        Map device address to host address.
        """

        return f"{return_val} = OMP_TARGET_ASSOCIATE_PTR({hst_ptr}, {dev_ptr}, {siz}, {offset}, {dev_id})"

    @classmethod
    def unmap_device_addr(cls, hst_ptr, dev_id, return_val):
        """
        Unmap device memory associated to a given host address.
        """

        return f"{return_val} = OMP_TARGET_DISASSOCIATE_PTR({hst_ptr}, {dev_id})"
