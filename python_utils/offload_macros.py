# (C) Copyright 2022- ECMWF.
# (C) Copyright 2022- Meteo-France.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation
# nor does it submit to any jurisdiction.

import fypp
from offload_backends import NVHPCOpenACC, NVHPCOpenACCCUDA, HostOnly

"""
A common entry point for retrieving macros from the various GPU offload backends.
"""

_offload_map = {
    'NVHPCOpenACC': NVHPCOpenACC,
    'HostOnly': HostOnly,
    'NVHPCOpenACCCUDA': NVHPCOpenACCCUDA
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
            _wrapped_lines += [_format_lines(s, indent=indent, pragma=pragma),]
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

def runtime_api_import(indent=0):
    """
    Import the runtime API.
    """

    backend = _get_offload_backend()
    method = _get_method(backend, 'runtime_api_import')

    return _format_lines(method(), indent=indent)

def c_devptr_decl(symbols, indent=0):
    """
    Declare symbols of type `TYPE(C_DEVPTR)` (or equivalent).
    """

    backend = _get_offload_backend()
    method = _get_method(backend, 'c_devptr_decl')

    return _format_lines(method(symbols), indent=indent)

def host_data(use_device, indent=0):
    """
    Start a `host_data` (or equivalent) region.
    """

    backend = _get_offload_backend()
    method = _get_method(backend, 'host_data')

    return _format_lines(method(use_device), indent=indent, pragma=backend.pragma)

def end_host_data(indent=0):
    """
    End a `host_data` (or equivalent) region.
    """

    backend = _get_offload_backend()
    method = _get_method(backend, 'end_host_data')

    return _format_lines(method(), indent=indent)

def devptr_cloc(symbol, indent=0):
    """
    Get the C address of a device variable.
    """

    backend = _get_offload_backend()
    method = _get_method(backend, 'devptr_c_loc')

    return _format_lines(method(symbol))

def memcpy_to_device(dev, host, size, indent=0):
    """
    Copy a contiguous section of data from host to device.
    """

    backend = _get_offload_backend()
    method = _get_method(backend, 'memcpy_to_device')

    return _format_lines(method(dev, host, size), indent=indent)

def memcpy_to_device_async(dev, host, size, queue, indent=0):
    """
    Asynchronously copy a contiguous section of data from host to device.
    """

    backend = _get_offload_backend()
    method = _get_method(backend, 'memcpy_to_device_async')

    return _format_lines(method(dev, host, size, queue), indent=indent)

def memcpy_from_device(dev, host, size, indent=0):
    """
    Copy a contiguous section of data from device to host.
    """

    backend = _get_offload_backend()
    method = _get_method(backend, 'memcpy_from_device')

    return _format_lines(method(dev, host, size), indent=indent)

def memcpy_from_device_async(dev, host, size, queue, indent=0):
    """
    Asynchronously copy a contiguous section of data from device to host.
    """

    backend = _get_offload_backend()
    method = _get_method(backend, 'memcpy_from_device_async')

    return _format_lines(method(dev, host, size, queue), indent=indent)

def create(symbols, indent=0):
    """
    Allocate host-mapped memory on device.
    """

    backend = _get_offload_backend()
    method = _get_method(backend, 'create')

    return _format_lines(method(symbols), indent=indent)

def delete(symbols, indent=0):
    """
    Free host-mapped memory on device.
    """

    backend = _get_offload_backend()
    method = _get_method(backend, 'delete')

    return _format_lines(method(symbols), indent=indent)

def attach(ptr, indent=0):
    """
    Attach a device pointer to its target.
    """

    backend = _get_offload_backend()
    method = _get_method(backend, 'attach')

    return _format_lines(method(ptr), indent=indent)

def detach(ptr, indent=0):
    """
    Detach a device pointer from its target.
    """

    backend = _get_offload_backend()
    method = _get_method(backend, 'detach')

    return _format_lines(method(ptr), indent=indent)

def kernels(**kwargs):
    """
    Launch an implicitly mapped parallel kernel on device.
    """

    backend = _get_offload_backend()
    method = _get_method(backend, 'kernels')

    indent = kwargs.pop('indent', 0)
    return _format_lines(method(**kwargs), indent=indent)

def end_kernels(indent=0):
    """
    End an implicitly mapped parallel kernel on device.
    """

    backend = _get_offload_backend()
    method = _get_method(backend, 'end_kernels')

    return _format_lines(method(), indent=indent)

def async_wait(stream, indent=0):
    """
    Wait for the operations queued on a stream to complete.
    """

    backend = _get_offload_backend()
    method = _get_method(backend, 'async_wait')

    return _format_lines(method(stream), indent=indent)

def parallel_loop(**kwargs):
    """
    Launch an explicitly mapped parallel kernel on device.
    """

    backend = _get_offload_backend()
    method = _get_method(backend, 'parallel_loop')

    indent = kwargs.pop('indent', 0)
    return _format_lines(method(**kwargs), indent=indent)

def end_parallel_loop(indent=0):
    """
    End an explicitly mapped parallel kernel on device.
    """

    backend = _get_offload_backend()
    method = _get_method(backend, 'end_parallel_loop')

    return _format_lines(method(), indent=indent)

def annotate_loop(**kwargs):
    """
    Annotate a loop in a device parallel region.
    """

    backend = _get_offload_backend()
    method = _get_method(backend, 'annotate_loop')

    indent = kwargs.pop('indent', 0)
    return _format_lines(method(**kwargs), indent=indent)

def declare(**kwargs):
    """
    Issue a device declaration for a host-mapped symbol.
    """

    backend = _get_offload_backend()
    method = _get_method(backend, 'declare')

    indent = kwargs.pop('indent', 0)
    return _format_lines(method(**kwargs), indent=indent)

def serial(**kwargs):
    """
    Launch a serial kernel on device.
    """

    backend = _get_offload_backend()
    method = _get_method(backend, 'launch_serial_kernel')

    indent = kwargs.pop('indent', 0)
    return _format_lines(method(**kwargs), indent=indent)

def end_serial(indent=0):
    """
    End a serial device kernel.
    """

    backend = _get_offload_backend()
    method = _get_method(backend, 'end_serial_kernel')

    return _format_lines(method(), indent=indent)

def update_device(symbols, indent=0):
    """
    Update host-mapped symbol on device.
    """

    backend = _get_offload_backend()
    method = _get_method(backend, 'update_device')

    return _format_lines(method(symbols), indent=indent)

def update_host(symbols, indent=0):
    """
    Update device-mapped symbol on host.
    """

    backend = _get_offload_backend()
    method = _get_method(backend, 'update_host')

    return _format_lines(method(symbols), indent=indent)

def stream_handle_kind():
    """
    Return the INTEGER kind specifier for a stream handle.
    """

    backend = _get_offload_backend()
    method = _get_method(backend, 'stream_handle_kind')

    return _format_lines(method())

def data(**kwargs):
    """
    Start a `data` (or equivalent) region.
    """

    backend = _get_offload_backend()
    method = _get_method(backend, 'data')

    indent = kwargs.pop('indent', 0)
    return _format_lines(method(**kwargs), indent=indent, pragma=backend.pragma)

def end_data(indent=0):
    """
    End a `data` (or equivalent) region.
    """

    backend = _get_offload_backend()
    method = _get_method(backend, 'data_end')

    return _format_lines(method(), indent=indent)

def dev_malloc_intf(indent=0):
    """
    The ISO_C interface for a device memory allocation.
    """

    backend = _get_offload_backend()
    method = _get_method(backend, 'dev_malloc_intf')

    return _format_lines(method(), indent=indent)

def dev_free_intf(indent=0):
    """
    The ISO_C interface for freeing device memory.
    """

    backend = _get_offload_backend()
    method = _get_method(backend, 'dev_free_intf')

    return _format_lines(method(), indent=indent)

def runtime_error_decl(symbols, indent=0):
    """
    Declaration for the variable used to store the runtime API error status.
    """

    backend = _get_offload_backend()
    method = _get_method(backend, 'runtime_error_return_type_decl')

    return _format_lines(method(symbols), indent=indent)

def dev_malloc(ptr, size, return_val="ISTAT", indent=0):
    """
    Allocate memory on device.
    """

    backend = _get_offload_backend()
    method = _get_method(backend, 'dev_malloc')

    return _format_lines(method(ptr, size, return_val=return_val), indent=indent)

def dev_free(ptr, return_val="ISTAT", indent=0):
    """
    Free device memory.
    """

    backend = _get_offload_backend()
    method = _get_method(backend, 'dev_free')

    return _format_lines(method(ptr, return_val=return_val), indent=indent)

def register_host_set_flags(flag_var, val, indent=0):
    """
    Set flags for page-locking host memory.
    """

    backend = _get_offload_backend()
    method = _get_method(backend, 'register_host_set_flags')

    return _format_lines(method(flag_var, val), indent=indent)

def register_host_decl_flags(flag_var, indent=0):
    """
    Declare variable used to store flags for controlling page-locking of host memory.
    """

    backend = _get_offload_backend()
    method = _get_method(backend, 'register_host_decl_flags')

    return _format_lines(method(flag_var), indent=indent)

def register_host(ptr, size, flags, return_val="ISTAT", indent=0):
    """
    Page-lock host memory.
    """

    backend = _get_offload_backend()
    method = _get_method(backend, 'register_host')

    return _format_lines(method(ptr, size, flags, return_val=return_val), indent=indent)

def unregister_host(ptr, return_val="ISTAT", indent=0):
    """
    Unpin (i.e. undo page-locking) host memory.
    """

    backend = _get_offload_backend()
    method = _get_method(backend, 'unregister_host')

    return _format_lines(method(ptr, return_val=return_val), indent=indent)

def host_register_intf(indent=0):
    """
    The ISO_C interface for page-locking host memory.
    """

    backend = _get_offload_backend()
    method = _get_method(backend, 'host_register_intf')

    return _format_lines(method(), indent=indent)

def host_unregister_intf(indent=0):
    """
    The ISO_C interface for un-pinning (i.e. undo page-locking) host memory.
    """

    backend = _get_offload_backend()
    method = _get_method(backend, 'host_unregister_intf')

    return _format_lines(method(), indent=indent)

def set_async_stream(queue, stream, indent=0):
    """
    Set an asynchronous stream.
    """

    backend = _get_offload_backend()
    method = _get_method(backend, 'set_async_stream')

    return _format_lines(method(queue, stream), indent=indent)

def memcpy_2D(src, src_pitch, dst, dst_pitch, width, height, return_val="ISTAT", indent=0):
    """
    Copy a strided memory region from source (src) to destination (dst).
    """

    backend = _get_offload_backend()
    method = _get_method(backend, 'memcpy_2D')

    return _format_lines(method(src, src_pitch, dst, dst_pitch, width, height, return_val=return_val),
                         indent=indent)

def memcpy_2D_async(src, src_pitch, dst, dst_pitch, width, height, stream, return_val="ISTAT", indent=0):
    """
    Asynchronously copy a strided memory region from source (src) to destination (dst).
    """

    backend = _get_offload_backend()
    method = _get_method(backend, 'copy_2D_async')

    return _format_lines(method(src, src_pitch, dst, dst_pitch, width, height, stream, return_val=return_val),
                         indent=indent)
