# Snowy

Snowy is a UCI chess engine written in C++. It uses an array-based board
representation (ie, not bitboard), and searches in a straightforward minimax
manner, with no depth extensions or reductions. The playing strength has not
been precisely evaluated, but it is somewhere between
[TSCP](http://www.tckerrigan.com/Chess/TSCP/) and
[faile](http://faile.sourceforge.net/).

It is available under the terms of the MIT license.

## Acknowledgements

This engine would not have been possible without the knowledge freely shared by
others:

* The truly excellent [Chess Programming Wiki](https://chessprogramming.wikispaces.com/)
* The [Computer Chess Club](http://www.talkchess.com/forum/index.php)
* The simple and approachable [TSCP](http://www.tckerrigan.com/Chess/TSCP/) engine
* The strong and very well commented [Crafty](http://www.craftychess.com/) engine

If you pursue only one of those resources, let it be the [Chess Programming
Wiki](https://chessprogramming.wikispaces.com/). I cannot overstate what a
great help it has been to a beginner like myself.
