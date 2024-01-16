# Snowy

Snowy is a UCI chess engine written in C++. It uses an array-based board
representation (ie, not bitboard), and searches in a straightforward minimax
manner, with no depth extensions or reductions. The playing strength has not
been precisely evaluated, but it is somewhere between
[faile](http://faile.sourceforge.net/) and
[Vice](http://bluefever.net/Downloads/ViceReadMe.html).

It is available under the terms of the MIT license.

## Binaries

Windows binaries can be found in the [Releases section](https://github.com/JasonCreighton/snowy/releases).

## Building

For portability, [CMake](https://cmake.org/) is used to generate
platform-specific build configurations.

On Linux, you can build and run with:

```
$ cmake -B out/Release -DCMAKE_BUILD_TYPE=Release
$ cd out/Release
$ make
$ ./snowy
```

On Windows, recent versions of Visual Studio can open a directory containing a
CMakeLists.txt file directly with no configuration required. Alternatively,
if you wish to use a more traditional Visual Studio solution, start "x64
Native Tools Command Prompt for VS" (to get the cmake bundled with VS into
your $PATH) and run:

```
$ cmake -B out
```

This will generate a Visual Studio solution in the "out" directory.

## Acknowledgements

This engine would not have been possible without the knowledge freely shared by
others:

* The truly excellent [Chess Programming Wiki](https://www.chessprogramming.org/Main_Page)
* The [Computer Chess Club](http://www.talkchess.com/forum/index.php)
* The simple and approachable [TSCP](http://www.tckerrigan.com/Chess/TSCP/) engine
* The strong and very well commented [Crafty](http://www.craftychess.com/) engine

If you pursue only one of those resources, let it be the [Chess Programming
Wiki](https://www.chessprogramming.org/Main_Page). I cannot overstate what a
great help it has been to a beginner like myself.
