# (C) Copyright 2022- ECMWF.
# (C) Copyright 2022- Meteo-France.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation
# nor does it submit to any jurisdiction.

execute_process(COMMAND "./init_owner.x"
                RESULT_VARIABLE EXIT_CODE
                OUTPUT_VARIABLE STDOUT
                COMMAND_ERROR_IS_FATAL ANY)

if( NOT EXIT_CODE EQUAL 0 )
   message(FATAL_ERROR "init_owner unit-test failed")
endif()

string(FIND ${STDOUT} "Failed to allocate page-locked memory" RESULT)
if( NOT RESULT EQUAL -1 )
   message(FATAL_ERROR "Failed to allocate page-locked memory")
endif()