# (C) Copyright 2022- ECMWF.
# (C) Copyright 2022- Meteo-France.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation
# nor does it submit to any jurisdiction.

# Source me to get the correct configure/build/run environment

module purge
module load cmake/3.24.1
module load nvhpc/23.9
module load python/3.7.6

DIR_PATH=$(realpath $(dirname $BASH_SOURCE))
export CMAKE_TOOLCHAIN_FILE=$DIR_PATH/toolchain.cmake
