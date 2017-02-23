# Copyright © 2017 Stéphane Adjemian <stepan@dynare.org>
#
# This file is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This file is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# GPLv3 license is available at <http://www.gnu.org/licenses/>.

include ./configure.inc

ROOT_PATH := ${CURDIR}

all: binaries/linux/32/x13 binaries/linux/64/x13 binaries/windows/32/x13.exe binaries/windows/64/x13.exe
	rm -rf tmp

binaries/linux/32/x13: src/Makefile
	mkdir -p tmp/linux/32
	cp src/* tmp/linux/32
	cd tmp/linux/32; make -j4 FC=gfortran LINKER=gfortran FFLAGS="-O2 -m32" LDFLAGS="-s -m32" PROGRAM=x13
	cp tmp/linux/32/x13 binaries/linux/32/x13
	rm -rf tmp/linux/32

binaries/linux/64/x13: src/Makefile
	mkdir -p tmp/linux/64
	cp src/* tmp/linux/64
	cd tmp/linux/64; make -j4 FC=gfortran LINKER=gfortran FFLAGS="-O2 -m64" PROGRAM=x13
	cp tmp/linux/64/x13 binaries/linux/64/x13
	rm -rf tmp/linux/64

binaries/windows/32/x13.exe: src/Makefile
	mkdir -p tmp/windows/32
	cp src/* tmp/windows/32
	cd tmp/windows/32; make FC=i686-w64-mingw32-gfortran LINKER=i686-w64-mingw32-gfortran FFLAGS=-O1 PROGRAM=x13.exe
	cp tmp/windows/32/x13.exe binaries/windows/32/x13.exe
	rm -rf tmp/windows/32

binaries/windows/64/x13.exe: src/Makefile
	mkdir -p tmp/windows/64
	cp src/* tmp/windows/64
	cd tmp/windows/64; make FC=x86_64-w64-mingw32-gfortran LINKER=x86_64-w64-mingw32-gfortran FFLAGS=-O1 PROGRAM=x13.exe
	cp tmp/windows/64/x13.exe binaries/windows/64/x13.exe
	rm -rf tmp/windows/64

clean-tarballs:
	rm src/*.tar.gz

clean-sources:
	rm src/*.i src/*.f src/*.prm src/*.cmn src/*.var src/Makefile

clean-binaries:
	rm -f binaries/linux/32/x13 binaries/linux/64/x13 binaries/windows/32/x13.exe binaries/windows/64/x13.exe

clean-all: clean-tarballs clean-sources clean-binaries

src/${SRC_REMOTE_FILE}:
	cd src; wget ${SRC_REMOTE_ADDRESS}${SRC_REMOTE_FILE}

src/Makefile: src/${SRC_REMOTE_FILE}
	cd src; tar xvf ${SRC_REMOTE_FILE}; mv makefile.gf Makefile; patch Makefile < ../patches/Makefile.patch
