# (C) Copyright 2022- ECMWF.
# (C) Copyright 2022- Meteo-France.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation
# nor does it submit to any jurisdiction.

add_subdirectory( debug )
add_subdirectory( core )
add_subdirectory( factory )
if( HAVE_FIELD_GANG )
  add_subdirectory( buffer )
endif()
add_subdirectory( util )
add_subdirectory( shuffle )
if( HAVE_IO )
  add_subdirectory( io )
endif()
