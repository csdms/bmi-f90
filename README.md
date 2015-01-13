# bmi-f90
Fortran bindings for the Basic Modeling Interface

Build
-----
To build the BMI Fortran-bindings and tests,

    $ mkdir _build && cd _build
    $ cmake ../ -DCMAKE_INSTALL_PREFIX=<path-to-installation>
    $ make

where `<path-to-installation>` is the base directory where you want
to install things (`/usr/local` is the default). To install,

    $ make install

Run some simple tests with,

    $ make test
