Compilation of x13 for linux/windows/osx 32/64 bits targets is triggered by the following command in the root directory:
```shell
~$ make -j4
```
The only requirements are `make`, `gfortran`, `mingw-w64`, `clang`, `patch`, and `libxml2-dev`. The `push` rule is available only if the variables `REMOTE_SERVER` and `REMOTE_PATH` are set in a file called `remote.inc`.

For the OSX targets [osxcross](https://github.com/tpoechtrager/osxcross), which is provided as a submodule, needs to be installed and compiled: 
```shell
~$ git submodule update --init
~$ make osxcross
```
The last command will compile the `gfortran` cross-compiler for OSX targets.
