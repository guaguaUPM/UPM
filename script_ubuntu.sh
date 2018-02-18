#!/bin/bash
sudo apt -y update
sudo apt -u upgrade
sudo apt -y install build-essential gfortran

wget http://www.netlib.org/lapack/lapack-3.8.0.tar.gz
tar -xvzf lapack-3.8.0.tar.gz
cd lapack*
