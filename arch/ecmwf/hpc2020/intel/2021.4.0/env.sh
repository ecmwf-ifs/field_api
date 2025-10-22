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

# Unload all modules to be certain
module_unload intel
module_unload openmpi
module_unload hpcx-openmpi
module_unload hdf5
module_unload cmake
module_unload python3

# Load modules
module_load prgenv/intel
module_load intel/2021.4.0
module_load hpcx-openmpi/2.18.1
module_load hdf5/1.12.2
module_load cmake/4.0.2
module_load python3/3.11.10-01
module_load ecbuild/3.12.0
module_load ninja/1.12.1


set -x

# Restore tracing to stored setting
{ if [[ -n "$tracing_" ]]; then set -x; else set +x; fi } 2>/dev/null

