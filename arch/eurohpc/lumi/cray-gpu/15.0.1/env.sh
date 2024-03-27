# (C) Copyright 2022- ECMWF.
# (C) Copyright 2022- Meteo-France.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation
# nor does it submit to any jurisdiction.

# Source me to get the correct configure/build/run environment

# Store tracing and disable (module is *way* too verbose)
{ tracing_=${-//[^x]/}; set +x; } 2>/dev/null

module_load() {
  echo "+ module load $1"
  module load $1
}
module_unload() {
  echo "+ module unload $1"
  module unload $1
}
module_purge() {
  echo "+ all existing modules unloaded"
  module purge
}

# Unload all modules to be certain
module_purge

# Load modules
module_load PrgEnv-cray/8.3.3
module_load LUMI/23.03
module_load rocm/5.2.3
module_load cce/15.0.1
module_load cray-libsci/22.08.1.1
module_load craype/2.7.20
module_load cray-mpich/8.1.18
module_load craype-accel-amd-gfx90a
module_load buildtools/23.03
module_load cray-python/3.9.12.1
module_load partition/G

# Increase stack size to maximum
ulimit -S -s unlimited
ulimit -S -l unlimited

# Export environment variable3s
export MPI_HOME=${MPICH_DIR}
export CC=cc
export CXX=CC
export FC=ftn

# Restore tracing to stored setting
{ if [[ -n "$tracing_" ]]; then set -x; else set +x; fi } 2>/dev/null

path=$BASH_SOURCE
DIR_PATH=$(dirname $path)
export ECBUILD_TOOLCHAIN=$DIR_PATH/toolchain.cmake
