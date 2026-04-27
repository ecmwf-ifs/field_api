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
   elseif( FYPP MATCHES FYPP-NOTFOUND OR (fckit_FOUND AND (NOT fckit_HAVE_FCKIT_VENV OR NOT DEFINED fckit_HAVE_FCKIT_VENV)) )
     # Discover only system install Python 3
     set( Python3_FIND_VIRTUALENV STANDARD )
     find_package( Python3 COMPONENTS Interpreter )

     execute_process( COMMAND ${Python3_EXECUTABLE} -m ensurepip --upgrade OUTPUT_QUIET )
     execute_process( COMMAND ${Python3_EXECUTABLE} -m pip --disable-pip-version-check install fypp OUTPUT_QUIET )
     ecbuild_info("field_api installed fypp as a pip package")
     set( FYPP ${Python3_EXECUTABLE} -m fypp )
   endif()

endmacro()
