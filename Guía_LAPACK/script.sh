#!/bin/sh
sudo apt -y update
sudo apt -y upgrade
sudo apt -y install make build-essential gfortran

wget http://www.netlib.org/lapack/lapack-3.8.0.tar.gz
tar -xvzf lapack-3.8.0.tar.gz
cd lapack-3.8.0

echo "LISTO"
