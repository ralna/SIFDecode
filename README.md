# SIFDecode
A package to decode SIF optimization test examples for use by CUTEst and GALAHAD

[![Build Status](https://img.shields.io/github/actions/workflow/status/ralna/SIFDecode/ci.yml?branch=master)](https://github.com/ralna/SIFDecode/actions/workflows/ci.yml)
[![License: LGPL v3](https://img.shields.io/badge/License-LGPL%20v3-blue.svg)](https://www.gnu.org/licenses/lgpl-3.0)

## Installation

### Precompiled library and executables

We provide precompiled SIFDecode executables in the [releases tab](https://github.com/ralna/SIFDecode/releases/latest/) for Linux (x64 and aarch64), macOS (x64 and aarch64), and Windows (x64).

### Installation from source

SIFDecode can be installed using the [Meson build system](https://mesonbuild.com) (all commands below are to be run from the top of the source tree):

```shell
meson setup builddir
meson compile -C builddir
meson install -C builddir
```

SIFDecode can also be installed via the "make" build system based on [ARCHDefs](https://github.com/ralna/ARCHDefs).
To use this variant, follow the instructions in the CUTEst [wiki](https://github.com/ralna/CUTEst/wiki).

## Where to find SIF files
A large collection of SIF files can be found at https://bitbucket.org/optrove/workspace/repositories/.
In particular this contains:
* The CUTEst NLP test set in the `sif` repository
* The Maros-Meszaros QP test set in the `maros-meszaros` repository
* The Netlib LP test set in the `netlib-lp` repository

The test problems are classified according to the [CUTE classification scheme](https://ralna.github.io/SIFDecode/html/classification/).

## How to decode a SIF file

Since version `v3.0.0`, an executable binary `sifdecoder` is
available through the Meson build system and allows you to easily
decode SIF files on any platform.

```shell
sifdecoder -h  # display the options
sifdecoder -sp ROSENBR.SIF  # decode the SIF problem in single precision
sifdecoder -dp ROSENBR.SIF  # decode the SIF problem in double precision
sifdecoder -qp ROSENBR.SIF  # decode the SIF problem in quadruple precision
```

Then, you can create either a shared or a static library for your problem
using a Fortran compiler, such as `gfortran`.
```shell
gfortran -O3 -shared -fPIC -o libROSENBR.so *.f     # shared library on Linux and FreeBSD
gfortran -O3 -shared -fPIC -o libROSENBR.dll *.f    # shared library on Windows
gfortran -O3 -shared -fPIC -o libROSENBR.dylib *.f  # shared library on Mac

gfortran -O3 -c *.f  # generate object files *.o
ar rcs libROSENBR.a *.o  # static library on all platform
```

The executable `sifdecoder` accepts the option `-suffix`.
All generated files are then suffixed with the problem name and the precision,
which allows SIF files to be decoded in parallel within the same folder.

A bash script [`sifdecoder`](https://github.com/ralna/SIFDecode/blob/master/bin/sifdecoder) can
also be used under the "make" build system with additional options but is less interoperable.
