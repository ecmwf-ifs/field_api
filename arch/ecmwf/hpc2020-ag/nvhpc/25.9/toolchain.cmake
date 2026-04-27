# (C) Copyright 2022- ECMWF.
# (C) Copyright 2022- ECMWF.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation
# nor does it submit to any jurisdiction.

####################################################################
# OpenACC FLAGS
####################################################################

set( OpenACC_Fortran_FLAGS "-acc=gpu -gpu=cc90" CACHE STRING "" )

####################################################################
# OpenMP FLAGS
####################################################################

set( OpenMP_Fortran_FLAGS "-mp=gpu -gpu=cc90" CACHE STRING "" )

# Necessary when CUDA math libs are installed in a different location to cudart
set( CMAKE_EXE_LINKER_FLAGS "-L$ENV{NVHPC_ROOT}/math_libs/13.0/lib64" )
set( CMAKE_SHARED_LINKER_FLAGS "-L$ENV{NVHPC_ROOT}/math_libs/13.0/lib64" )
