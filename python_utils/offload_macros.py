# (C) Copyright 2022- ECMWF.
# (C) Copyright 2022- Meteo-France.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation
# nor does it submit to any jurisdiction.

import fypp
from offload_backends import NVHPCOpenACC, HostOnly

"""
A common entry point for retrieving macros from the various GPU offload backends.
"""

_offload_map = {
    'NVHPCOpenACC': NVHPCOpenACC,
    'HostOnly': HostOnly
}

def _wrap_lines(input_str, ref_len, pragma='', indent=0):
    """
    Wrap a long line.

    Parameters
    ----------
    input_str : str
       The long line to wrap.
    ref_len : int
       The maximum permissible line length.
    pragma : str
       The pragma keyword to start new lines with.
    indent : int
       The length of the indent to place at the start of each line.
    """

    pieces = input_str.split(' ')

    count = 0
    lines = [' ' * indent,]
    for piece in pieces:
        if len(lines[count]) + len(piece) > ref_len:
            lines[count] += ' &'
            lines += [' ' * indent + pragma + ' &',]
            count += 1

        lines[count] += (piece + ' ')

    return lines


def _format_lines(input_str, indent=0, width=132, pragma=''):
    """
    Add a specified indent to an input string and wrap it across multiple lines if it
    exceeds the specified width.

    Parameters
    ----------
    input_str : str
       The input string to format.
    indent : int
       The size of the indent to apply before the line.
    width : int
       The maximum allowed length of a line.
    pragma : str
       A pragma keyword to prepend to a new line.
    """

    _wrapped_lines = []
    if isinstance(input_str, (list, tuple)):
        for s in input_str:
            _wrapped_lines += _format_lines(s, indent=indent, pragma=pragma)
    else:
        ref_len = width - indent - 2 - len(pragma)
        if len(input_str) > width - indent:
            _wrapped_lines += _wrap_lines(input_str, ref_len, indent=indent, pragma=pragma)
        else:
            _wrapped_lines += [' ' * indent + input_str,]

    return '\n'.join(_wrapped_lines)

def _get_offload_backend():
    """
    Determine the specific offload backend to be used.
    """

    optparser = fypp.get_option_parser()
    options, _ = optparser.parse_args()

    offload_model = [opt for opt in options.defines if 'OFFLOAD_MODEL' in opt][0]
    offload_model = offload_model.split('=')[-1].replace('"', '')

    return _offload_map[offload_model]

def _empty_string(*args, **kwargs):
    """Simple method to return an empty string."""
    return ""

def _get_method(backend, method):
    """
    Retrieve the appropriate method from the given backend.
    """

    try:
        return getattr(backend, method)
    except AttributeError:
        return _empty_string

def RuntimeApiImport(indent=0):
    """
    Import the runtime API.
    """

    backend = _get_offload_backend()
    method = _get_method(backend, 'runtime_api_import')

    return _format_lines(method())

def CDevptrDecl(symbols, indent=0):
    """
    Declare symbols of type `TYPE(C_DEVPTR)` (or equivalent).
    """

    backend = _get_offload_backend()
    method = _get_method(backend, 'c_devptr_declaration')

    return _format_lines(method(symbols))

def HostDataStart(symbols, indent=0):
    """
    Start a `host_data` (or equivalent) region.
    """

    backend = _get_offload_backend()
    method = _get_method(backend, 'host_data_start')

    return _format_lines(method(symbols), indent=indent, pragma=backend.pragma)

def HostDataEnd(indent=0):
    """
    End a `host_data` (or equivalent) region.
    """

    backend = _get_offload_backend()
    method = _get_method(backend, 'host_data_end')

    return _format_lines(method(), indent=indent)

def DevptrCLOC(symbol, indent=0):
    """
    Get the C address of a device variable.
    """

    backend = _get_offload_backend()
    method = _get_method(backend, 'devptr_c_loc')

    return _format_lines(method(symbol))

def CopyToDevice1D(dev, host, size, indent=0):
    """
    Copy a contiguous section of data from host to device.
    """

    backend = _get_offload_backend()
    method = _get_method(backend, 'copy_to_device_1D')

    return _format_lines(method(dev, host, size), indent=indent)

def CopyToDevice1DAsync(dev, host, size, queue, indent=0):
    """
    Asynchronously copy a contiguous section of data from host to device.
    """

    backend = _get_offload_backend()
    method = _get_method(backend, 'copy_to_device_1D_async')

    return _format_lines(method(dev, host, size, queue), indent=indent)

def CopyFromDevice1D(dev, host, size, indent=0):
    """
    Copy a contiguous section of data from device to host.
    """

    backend = _get_offload_backend()
    method = _get_method(backend, 'copy_from_device_1D')

    return _format_lines(method(dev, host, size), indent=indent)

def CopyFromDevice1DAsync(dev, host, size, queue, indent=0):
    """
    Asynchronously copy a contiguous section of data from device to host.
    """

    backend = _get_offload_backend()
    method = _get_method(backend, 'copy_from_device_1D_async')

    return _format_lines(method(dev, host, size, queue), indent=indent)

def HostMappedDevAlloc(data, indent=0):
    """
    Allocate host-mapped memory on device.
    """

    backend = _get_offload_backend()
    method = _get_method(backend, 'host_mapped_dev_alloc')

    return _format_lines(method(data), indent=indent)

def HostMappedDevFree(data, indent=0):
    """
    Free host-mapped memory on device.
    """

    backend = _get_offload_backend()
    method = _get_method(backend, 'host_mapped_dev_free')

    return _format_lines(method(data), indent=indent)

def AttachDevPtr(ptr, indent=0):
    """
    Attach a device pointer to its target.
    """

    backend = _get_offload_backend()
    method = _get_method(backend, 'attach_dev_ptr')

    return _format_lines(method(ptr), indent=indent)

def DetachDevPtr(ptr, indent=0):
    """
    Detach a device pointer from its target.
    """

    backend = _get_offload_backend()
    method = _get_method(backend, 'detach_dev_ptr')

    return _format_lines(method(ptr), indent=indent)

def LaunchParallelKernel(**kwargs):
    """
    Launch an implicitly mapped parallel kernel on device.
    """

    backend = _get_offload_backend()
    method = _get_method(backend, 'launch_kernel')

    indent = kwargs.pop('indent', 0)
    return _format_lines(method(**kwargs), indent=indent)

def EndParallelKernel(indent=0):
    """
    End an implicitly mapped parallel kernel on device.
    """

    backend = _get_offload_backend()
    method = _get_method(backend, 'end_kernel')

    return _format_lines(method(), indent=indent)

def WaitAsyncStream(stream, indent=0):
    """
    Wait for the operations queued on a stream to complete.
    """

    backend = _get_offload_backend()
    method = _get_method(backend, 'async_wait')

    return _format_lines(method(stream), indent=indent)

def LaunchParallelLoop(**kwargs):
    """
    Launch an explicitly mapped parallel kernel on device.
    """

    backend = _get_offload_backend()
    method = _get_method(backend, 'launch_parallel_loop')

    indent = kwargs.pop('indent', 0)
    return _format_lines(method(**kwargs), indent=indent)

def EndParallelLoop(indent=0):
    """
    End an explicitly mapped parallel kernel on device.
    """

    backend = _get_offload_backend()
    method = _get_method(backend, 'end_parallel_loop')

    return _format_lines(method(), indent=indent)

def AnnotateParallelLoop(**kwargs):
    """
    Annotate a loop in a device parallel region.
    """

    backend = _get_offload_backend()
    method = _get_method(backend, 'annotate_parallel_loop')

    indent = kwargs.pop('indent', 0)
    return _format_lines(method(**kwargs), indent=indent)

def Declare(**kwargs):
    """
    Issue a device declaration for a host-mapped symbol.
    """

    backend = _get_offload_backend()
    method = _get_method(backend, 'declare')

    indent = kwargs.pop('indent', 0)
    return _format_lines(method(**kwargs), indent=indent)

def LaunchSerialKernel(**kwargs):
    """
    Launch a serial kernel on device.
    """

    backend = _get_offload_backend()
    method = _get_method(backend, 'launch_serial_kernel')

    indent = kwargs.pop('indent', 0)
    return _format_lines(method(**kwargs), indent=indent)

def EndSerialKernel(indent=0):
    """
    End a serial device kernel.
    """

    backend = _get_offload_backend()
    method = _get_method(backend, 'end_serial_kernel')

    return _format_lines(method(), indent=indent)

def UpdateDevice(data, indent=0):
    """
    Update host-mapped symbol on device.
    """

    backend = _get_offload_backend()
    method = _get_method(backend, 'update_device')

    return _format_lines(method(data), indent=indent)

def UpdateHost(data, indent=0):
    """
    Update device-mapped symbol on host.
    """

    backend = _get_offload_backend()
    method = _get_method(backend, 'update_host')

    return _format_lines(method(data), indent=indent)
