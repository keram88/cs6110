#! /bin/bash
set -e

wget https://github.com/msoos/cryptominisat/archive/4.5.3.tar.gz
tar xf 4.5.3.tar.gz
cd cryptominisat-4.5.3
mkdir build
cd build
cmake ../
make -j8
