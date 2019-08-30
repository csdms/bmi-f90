[![Build Status](https://travis-ci.org/csdms/bmi-f90.svg?branch=master)](https://travis-ci.org/csdms/bmi-f90)

# bmi-f90

Fortran 90 bindings for the CSDMS
[Basic Model Interface](https://bmi-spec.readthedocs.io) (BMI).

## Build

To build the bindings and tests on Linux and macOS, run

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

## Use

Run the heat model through its BMI with the `run_bmiheatf90` program,
which takes a model configuration file
(see the [testing](./testing) directory for a sample)
as a required parameter.
If `run_bmiheatf90` is in your path, run it with

    run_bmiheatf90 test.cfg

Output from the model is written to the file **bmiheatf90.out**
in the current directory.

## Note

Why two different Fortran BMIs?
Though Fortran 90/95 has the concept of an interface,
it doesn't allow procedures to be included within types.
This is difficult to reconcile with BMI, which, in Fortran,
would ideally be implemented as a collection of procedures in a type.
Thus, the Fortran 90/95 BMI is set up as an example
that a user can copy and modify,
substituting their code for code in the example.
This is somewhat cumbersome.
The Fortran 2003 BMI implementation acts a true interface--it can be imported
as a type from a module into a Fortran program and its methods overridden.
The CSDMS IF software engineers recommend using the Fortran 2003 bindings
in https://github.com/csdms/bmi-fortran;
however, we will continue to support the Fortran 90/95 bindings
for users in the CSDMS community who aren't comfortable
using the object-oriented features of Fortran 2003.
Further, both BMI implementations are backward-compatible with Fortran 77.
All that is needed is a compiler that's capable of handling
the more recent versions of Fortran;
for example `gfortran` in the GNU Compiler Collection.
