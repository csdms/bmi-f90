[![Build Status](https://travis-ci.org/csdms/bmi-f90.svg?branch=master)](https://travis-ci.org/csdms/bmi-f90)

# bmi-f90

Fortran 90 bindings for the Basic Model Interface.

Note that the bindings developed here
have been superceded by the Fortran 2003 bindings
in https://github.com/csdms/bmi-fortran.
CSDMS recommends using the Fortran 2003 bindings;
however, we've kept this repository active
in case you'd prefer to use these older bindings.

## Build

To build the BMI Fortran bindings and tests, execute

    $ mkdir _build && cd _build
    $ cmake .. -DCMAKE_INSTALL_PREFIX=<path-to-installation>
    $ make

where `<path-to-installation>` is the base directory where you want
to install things (`/usr/local` is the default).

## Install

To install:

    $ make install

## Test

Run some simple tests with

    $ make test
