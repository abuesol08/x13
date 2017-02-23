Compilation of x13 for linux/windows 32/64 bits targets is triggered by the following command in the root directory:
```shell
~$ make -j4
```
The only requirements are `make`, `gfortran` and `mingw-w64`. The `push` rule is available only if the variables `REMOTE_SERVER` and `REMOTE_PATH` are set in a file called `remote.inc`.
