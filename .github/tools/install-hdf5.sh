#!/bin/bash
set -euo pipefail
set -x

hdf5_version=1.14.6

# Choose hdf5
version_parts=($(echo ${hdf5_version} | tr "." "\n"))
major_version=${version_parts[0]}.${version_parts[1]}
major__version=${version_parts[0]}\_${version_parts[1]}
full_version=${version_parts[0]}\_${version_parts[1]}\_${version_parts[2]}
temporary_files="${RUNNER_TEMP}/hdf5"
url=https://support.hdfgroup.org/releases/hdf5/v${major__version}/v${full_version}/downloads/hdf5-${hdf5_version}.tar.gz

# Download hdf5
mkdir -p "${temporary_files}"
curl --location "$url" | tar zx -C "${temporary_files}"
cd ${temporary_files}
cd hd*
pwd
# Build hdf5
prefix="${GITHUB_WORKSPACE}/hdf5-install"
mkdir -p "${prefix}"
mkdir build
cd build
#./configure --prefix="${prefix}" --enable-shared --enable-fortran --enable-hl
#make -j
#make install
cmake -G Ninja .. -DCMAKE_INSTALL_PREFIX="${prefix}"  -DHDF5_BUILD_FORTRAN=ON -DHDF5_BUILD_HL_LIB=ON -DBUILD_TESTING=OFF
ninja
ninja install 

exit 0

