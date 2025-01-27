# (C) Copyright 2022- ECMWF.
# (C) Copyright 2022- Meteo-France.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation
# nor does it submit to any jurisdiction.

import fypp
from offload_backends import NVHPCOpenACC

"""
A common entry point for retrieving macros from the various GPU offload backends.
"""

_offload_map = {
    'NVHPCOpenACC': NVHPCOpenACC
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

def RuntimeApiImport(indent=0):
    """
    Import the runtime API.
    """

    backend = _get_offload_backend()

    return _format_lines(backend.runtime_api_import())

def CDevptrDecl(symbols, indent=0):
    """
    Declare symbols of type `TYPE(C_DEVPTR)` (or equivalent).
    """

    backend = _get_offload_backend()

    return _format_lines(backend.c_devptr_declaration(symbols))

def HostDataStart(symbols, indent=0):
    """
    Start a `host_data` (or equivalent) region.
    """

    backend = _get_offload_backend()
    return _format_lines(backend.host_data_start(symbols), indent=indent, pragma=backend.pragma)

def HostDataEnd(indent=0):
    """
    End a `host_data` (or equivalent) region.
    """

    backend = _get_offload_backend()
    return _format_lines(backend.host_data_end(), indent=indent)

def DevptrCLOC(symbol, indent=0):
    """
    Get the C address of a device variable.
    """

    backend = _get_offload_backend()
    return _format_lines(backend.devptr_c_loc(symbol))

def CopyToDevice1D(dev, host, size, indent=0):
    """
    Copy a contiguous section of data from host to device.
    """

    backend = _get_offload_backend()
    return _format_lines(backend.copy_to_device_1D(dev, host, size), indent=indent)

def CopyToDevice1DAsync(dev, host, size, queue, indent=0):
    """
    Asynchronously copy a contiguous section of data from host to device.
    """

    backend = _get_offload_backend()
    return _format_lines(backend.copy_to_device_1D_async(dev, host, size, queue), indent=indent)

def CopyFromDevice1D(dev, host, size, indent=0):
    """
    Copy a contiguous section of data from device to host.
    """

    backend = _get_offload_backend()
    return _format_lines(backend.copy_from_device_1D(dev, host, size), indent=indent)

def CopyFromDevice1DAsync(dev, host, size, queue, indent=0):
    """
    Asynchronously copy a contiguous section of data from device to host.
    """

    backend = _get_offload_backend()
    return _format_lines(backend.copy_from_device_1D_async(dev, host, size, queue), indent=indent)
