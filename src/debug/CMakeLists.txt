# (C) Copyright 2022- ECMWF.
# (C) Copyright 2022- Meteo-France.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation
# nor does it submit to any jurisdiction.

check_symbol_exists(backtrace execinfo.h HAVE_BACKTRACE)

list(APPEND srcs
    field_statistics_module.F90
    field_backtrace.c)

unset( _definitions )
if( HAVE_BACKTRACE )
   list(APPEND _defintions HAVE_BACKTRACE)
endif()

## define object library
field_api_add_object_library(
    LIBNAME      field_api_debug
    SRCS         ${srcs}
    DEFINITIONS  ${_definitions}
)
