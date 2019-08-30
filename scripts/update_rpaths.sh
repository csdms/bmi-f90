#!/usr/bin/env bash
# Updates runtime paths for executables on macOS.

if [[ -z "$CONDA_PREFIX" ]]; then
    CONDA_PREFIX=`python -c "import sys; print(sys.prefix)"`
fi

run_install_name_tool() {
    install_name_tool \
        -change @rpath/libheatf90.dylib \
            ${CONDA_PREFIX}/lib/libheatf90.dylib \
        -change @rpath/libbmiheatf90.dylib \
            ${CONDA_PREFIX}/lib/libbmiheatf90.dylib \
        -change @rpath/libgfortran.3.dylib \
            ${CONDA_PREFIX}/lib/libgfortran.3.dylib \
        -change @rpath/libquadmath.0.dylib \
            ${CONDA_PREFIX}/lib/libquadmath.0.dylib \
        $1
    echo "- updated $1"
}

tests=`ls -1 ./testing/ | egrep "_test"`
for exe in $tests; do
    run_install_name_tool ./testing/$exe
done

run_install_name_tool ./heat/run_heatf90
run_install_name_tool ./heat/run_bmiheatf90
