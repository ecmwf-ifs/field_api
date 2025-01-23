# (C) Copyright 2022- ECMWF.
# (C) Copyright 2022- Meteo-France.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation
# nor does it submit to any jurisdiction.

##############################################################################
#.rst:
#
# field_api_find_fypp
# ===================
# 
# Find a compatible version of the fypp preprocessor.
#
#        field_api_find_fypp()
#
##############################################################################

macro( field_api_find_fypp )

   ecbuild_find_package( fckit )
   find_program( FYPP fypp )
   
   if( fckit_FOUND AND fckit_HAVE_FCKIT_VENV )
     set( FYPP ${FCKIT_VENV_EXE} -m fypp )
   elseif( fckit_FOUND )
     # This is only needed for building in environments with python3 older than 3.8
     list( APPEND _fckit_fypp_path "${FYPP}" )
     list( LENGTH _fckit_fypp_path _list_length )
     MATH( EXPR _last_entry "${_list_length} - 1" )
     list( GET _fckit_fypp_path ${_last_entry} FYPP )
   elseif( FYPP MATCHES FYPP-NOTFOUND )
     include(cmake/field_api_fetchcontent_fypp.cmake)
     set(FYPP ${fypp_SOURCE_DIR}/bin/fypp)
     ecbuild_info("fypp downloaded to: ${FYPP}")
   endif()

endmacro()
