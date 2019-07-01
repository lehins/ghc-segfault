# ghc-segfault
Minimal reproduction of the code that when compiled triggers a segfault at runtime


In order to trigger the bug:

* directly with GHC:

```
$ ghc -fforce-recomp -O1 app/Main.hs src/Complex.hs src/Sample.hs -o trigger-segfault
[1 of 3] Compiling Complex          ( src/Complex.hs, src/Complex.o )
[2 of 3] Compiling Sample           ( src/Sample.hs, src/Sample.o )
[3 of 3] Compiling Main             ( app/Main.hs, app/Main.o )
Linking trigger-segfault ...
$ ./trigger-segfault
Segmentation fault (core dumped)
```

* with stack:

```shell
$ stack run
lehins@lehins-HP:~/github/ghc-segfault$ stack run
segfault-0.1.0.0: unregistering (local file changes: app/Main.hs package.yaml segfault.cabal src/Complex.hs src/Sample.hs)
Building all executables for `segfault' once. After a successful build of all of them, only specified executables will be rebuilt.
segfault> configure (lib + exe)
Configuring segfault-0.1.0.0...
segfault> build (lib + exe)
Preprocessing library for segfault-0.1.0.0..
Building library for segfault-0.1.0.0..
[1 of 3] Compiling Complex
[2 of 3] Compiling Paths_segfault
[3 of 3] Compiling Sample
ignoring (possibly broken) abi-depends field for packages
Preprocessing executable 'gini-node' for segfault-0.1.0.0..
Building executable 'gini-node' for segfault-0.1.0.0..
[1 of 2] Compiling Main
[2 of 2] Compiling Paths_segfault
Linking .stack-work/dist/x86_64-linux/Cabal-2.2.0.1/build/gini-node/gini-node ...
segfault> copy/register
Installing library in /home/lehins/github/ghc-segfault/.stack-work/install/x86_64-linux/8071feca4dbf449a557015eb87d5e68fe80ec64f2471d9bc60e3f9d9f98f5e30/8.4.3/lib/x86_64-linux-ghc-8.4.3/segfault-0.1.0.0-9AgSU1erxpytV1E4TzaQv
Installing executable gini-node in /home/lehins/github/ghc-segfault/.stack-work/install/x86_64-linux/8071feca4dbf449a557015eb87d5e68fe80ec64f2471d9bc60e3f9d9f98f5e30/8.4.3/bin
Registering library for segfault-0.1.0.0..
Segmentation fault (core dumped)
```
