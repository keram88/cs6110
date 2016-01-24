#! /bin/bash
set -e

wget http://www.labri.fr/perso/lsimon/downloads/softwares/glucose-syrup.tgz
tar xf glucose-syrup.tgz
cd glucose-syrup
cd simp
make rs
