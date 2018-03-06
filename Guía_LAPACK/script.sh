#!/bin/sh
sudo apt -y update
sudo apt -y upgrade
sudo apt -y install make build-essential gfortran

wget http://www.netlib.org/lapack/lapack-3.8.0.tar.gz
tar -xvzf lapack-3.8.0.tar.gz
cd la*
cp INSTALL/make.inc.gfortran make.inc
make lib blaslib

sudo  cp liblapack.a /mnt/c
suudo cp librefblas.a /mnt/c

echo "LISTO"
