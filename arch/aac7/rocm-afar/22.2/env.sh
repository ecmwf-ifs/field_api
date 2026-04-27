# (C) Copyright 1988- ECMWF.
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

# Unload to be certain
module purge

# Load modules
module_load PrgEnv-cray/8.6.0
module_load cray-python/3.11.7

LD_LIBRARY_PATH=/shared/midgard/home/ahmad_nawab_dwj/rocm-afar-8873-drop-22.2.0/lib:$LD_LIBRARY_PATH
LD_LIBRARY_PATH=/shared/midgard/home/ahmad_nawab_dwj/rocm-afar-8873-drop-22.2.0/lib/llvm/lib:$LD_LIBRARY_PATH
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH
PATH=/shared/midgard/home/ahmad_nawab_dwj/rocm-afar-8873-drop-22.2.0/lib/llvm/bin:$PATH
PATH=/shared/midgard/home/ahmad_nawab_dwj/rocm-afar-8873-drop-22.2.0/bin:$PATH
export PATH=$PATH
#export hipfort_ROOT=/users/nawabahm/hipfort/install

# Export environment variable3s
export MPI_HOME=${MPICH_DIR}

export CC=amdclang CXX=amdclang++ FC=amdflang

module list

set -x

# Restore tracing to stored setting
{ if [[ -n "$tracing_" ]]; then set -x; else set +x; fi } 2>/dev/null

path=$BASH_SOURCE
DIR_PATH=$(dirname $path)
export ECBUILD_TOOLCHAIN=$DIR_PATH/toolchain.cmake
