ROOT_PATH := ${CURDIR}

all: binaries/linux/32/x13 binaries/linux/64/x13 binaries/windows/32/x13.exe binaries/windows/64/x13.exe

binaries/linux/32/x13:
	mkdir -p tmp/linux/32
	cp src/* tmp/linux/32
	cd tmp/linux/32; make -j4 FC=gfortran LINKER=gfortran FFLAGS="-O2 -m32" LDFLAGS="-s -m32" PROGRAM=x13
	cp tmp/linux/32/x13 binaries/linux/32/x13
	rm -rf tmp/linux/32

binaries/linux/64/x13:
	mkdir -p tmp/linux/64
	cp src/* tmp/linux/64
	cd tmp/linux/64; make -j4 FC=gfortran LINKER=gfortran FFLAGS="-O2 -m64" PROGRAM=x13
	cp tmp/linux/64/x13 binaries/linux/64/x13
	rm -rf tmp/linux/64

binaries/windows/32/x13.exe:
	mkdir -p tmp/windows/32
	cp src/* tmp/windows/32
	cd tmp/windows/32; make FC=i686-w64-mingw32-gfortran LINKER=i686-w64-mingw32-gfortran FFLAGS=-O1 PROGRAM=x13.exe
	cp tmp/windows/32/x13.exe binaries/windows/32/x13.exe
	rm -rf tmp/windows/32

binaries/windows/64/x13.exe:
	mkdir -p tmp/windows/64
	cp src/* tmp/windows/64
	cd tmp/windows/64; make FC=x86_64-w64-mingw32-gfortran LINKER=x86_64-w64-mingw32-gfortran FFLAGS=-O1 PROGRAM=x13.exe
	cp tmp/windows/64/x13.exe binaries/windows/64/x13.exe
	rm -rf tmp/windows/64

clean:
	rm -f binaries/linux/32/x13 binaries/linux/64/x13 binaries/windows/32/x13.exe binaries/windows/64/x13.exe
