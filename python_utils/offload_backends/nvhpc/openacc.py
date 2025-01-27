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
