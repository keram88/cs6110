#! /bin/bash
set -e

wget http://minisat.se/downloads/minisat-2.2.0.tar.gz
tar xf minisat-2.2.0.tar.gz
cd minisat
export MROOT=`pwd`
cd core
make rs

