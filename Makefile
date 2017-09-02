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

OSX_SDK_VERSION=10.11

include ./configure.inc

ifeq ($(wildcard remote.inc),)
REMOTE_CONFIGURATION_FILE=0
else
include ./remote.inc
REMOTE_CONFIGURATION_FILE=1
endif

ROOT_PATH := ${CURDIR}

.PHONY: all osxcross clean-tarballs clean-sources clean-binaries clean-archives clean-all clean-osxcross

all: binaries/linux/32/x13 binaries/linux/64/x13 binaries/windows/32/x13.exe binaries/windows/64/x13.exe binaries/osx/32/x13 binaries/osx/64/x13
	rm -rf tmp

osxcross: modules/osxcross/target/bin/x86_64-apple-darwin15-gfortran

modules/osxcross/target/bin/x86_64-apple-darwin15-gfortran: modules/osxcross/tarballs/MacOSX$(OSX_SDK_VERSION).sdk.tar.xz
	cd modules/osxcross; UNATTENDED=1  ./build.sh
	cd modules/osxcross; GCC_VERSION=6.4.0 ENABLE_FORTRAN=1 ./build_gcc.sh

modules/osxcross/tarballs/MacOSX$(OSX_SDK_VERSION).sdk.tar.xz:
	wget https://dynare.adjemian.eu/osx/$(OSX_SDK_VERSION)/sdk.tar.xz -O sdk.tar.xz
	mv sdk.tar.xz $@

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

binaries/osx/64/x13: src/Makefile
	mv modules/osxcross/target/x86_64-apple-darwin15/lib/libquadmath.dylib modules/osxcross/target/x86_64-apple-darwin15/lib/libquadmath.dylib.cache
	mv modules/osxcross/target/x86_64-apple-darwin15/lib/libquadmath.0.dylib modules/osxcross/target/x86_64-apple-darwin15/lib/libquadmath.0.dylib.cache
	mkdir -p tmp/osx/64
	cp src/* tmp/osx/64
	cd tmp/osx/64; patch Makefile < ../../../patches/Makefile.osx.patch; export PATH=$(ROOT_PATH)/modules/osxcross/target/bin:$(PATH);  make FC=x86_64-apple-darwin15-gfortran  FFLAGS="-m64" LINKER=x86_64-apple-darwin15-gfortran LDFLAGS="-static-libgcc -static-libgfortran"  PROGRAMM=x13
	cp tmp/osx/64/x13 binaries/osx/64/x13
	rm -rf tmp/osx/64
	mv modules/osxcross/target/x86_64-apple-darwin15/lib/libquadmath.dylib.cache modules/osxcross/target/x86_64-apple-darwin15/lib/libquadmath.dylib
	mv modules/osxcross/target/x86_64-apple-darwin15/lib/libquadmath.0.dylib.cache modules/osxcross/target/x86_64-apple-darwin15/lib/libquadmath.0.dylib

binaries/osx/32/x13: src/Makefile
	mv modules/osxcross/target/x86_64-apple-darwin15/lib/libquadmath.dylib modules/osxcross/target/x86_64-apple-darwin15/lib/libquadmath.dylib.cache
	mv modules/osxcross/target/x86_64-apple-darwin15/lib/libquadmath.0.dylib modules/osxcross/target/x86_64-apple-darwin15/lib/libquadmath.0.dylib.cache
	mkdir -p tmp/osx/32
	cp src/* tmp/osx/32
	cd tmp/osx/32; patch Makefile < ../../../patches/Makefile.osx.patch; export PATH=$(ROOT_PATH)/modules/osxcross/target/bin:$(PATH);  make FC=x86_64-apple-darwin15-gfortran  FFLAGS="-m32" LINKER=x86_64-apple-darwin15-gfortran LDFLAGS="-m32 -static-libgcc -static-libgfortran"  PROGRAMM=x13
	cp tmp/osx/32/x13 binaries/osx/32/x13
	rm -rf tmp/osx/32
	mv modules/osxcross/target/x86_64-apple-darwin15/lib/libquadmath.dylib.cache modules/osxcross/target/x86_64-apple-darwin15/lib/libquadmath.dylib
	mv modules/osxcross/target/x86_64-apple-darwin15/lib/libquadmath.0.dylib.cache modules/osxcross/target/x86_64-apple-darwin15/lib/libquadmath.0.dylib

src/${SRC_REMOTE_FILE}:
	cd src; wget ${SRC_REMOTE_ADDRESS}${SRC_REMOTE_FILE}

src/Makefile: src/${SRC_REMOTE_FILE}
	cd src; tar xvf ${SRC_REMOTE_FILE}; mv makefile.gf Makefile; patch Makefile < ../patches/Makefile.patch

x13.zip: all
	zip -r x13.zip binaries -x *.gitignore

x13.tar.xz: all
	XZ_OPT=-9 tar -c -c --exclude-from=.tarignore -Jvf x13.tar.xz binaries

push: x13.zip x13.tar.xz
ifeq ($(REMOTE_CONFIGURATION_FILE),1)
ifeq ($(REMOTE_SERVER),)
	@echo "You need to specify a remote server in the configuration file!"
else
ifeq ($(REMOTE_PATH),)
	@echo "You need to specify a remote path in the configuration file!"
else
	scp x13.zip ${REMOTE_SERVER}:${REMOTE_PATH}
	scp x13.tar.xz ${REMOTE_SERVER}:${REMOTE_PATH}
endif
endif
else
	@echo "The push rule is not available!"
	@echo "Please install the remote.inc configuration file."
endif

clean-tarballs:
	rm src/*.tar.gz

clean-sources:
	rm src/*.i src/*.f src/*.prm src/*.cmn src/*.var src/Makefile

clean-binaries:
	rm -f binaries/linux/32/x13 binaries/linux/64/x13 binaries/windows/32/x13.exe binaries/windows/64/x13.exe

clean-archives:
	rm *.zip *.tar.xz

clean-all: clean-tarballs clean-sources clean-binaries

clean-osxcross:
	cd modules/osxcross; git clean -xfd
