# Chess Kit (F#)
[![Build status](https://ci.appveyor.com/api/projects/status/20euy0r0xxsp19fx?svg=true)](https://ci.appveyor.com/project/marhoily/chesskitfs)
[![ChessKit.ChessLogic](https://img.shields.io/nuget/dt/ChessKit.ChessLogic.svg)](https://www.nuget.org/packages/ChessKit.ChessLogic/)

### What it is
V 1.0 is going to include:
 * Read [FEN](http://en.wikipedia.org/wiki/Forsyth%E2%80%93Edwards_Notation) into convenient Board class **done**
 * Read string encoded moves in [coordinate](http://en.wikipedia.org/wiki/Chess_notation) "e2-e4" (**done**) and [SAN](http://en.wikipedia.org/wiki/Algebraic_notation_(chess)) "Nxe4+" notations
 * Apply the moves to the board and not only get a new board, but also all possible info about that move, like why it is illegal, or that it gives check or mate. ( **partly done**)
 * Get all legal moves for a piece. This is not based on trying to apply moves from all squares to all squaresn, but a dedicated algorithm
 * Print the board nicely as text ;) (**done**)
 * Calculate a [Zobrist hash key](http://en.wikipedia.org/wiki/Zobrist_hashing) for a position
 
### What it is not 
 * it is not a chess engine
 * it's not nearly optimal enough to enable you to write a chess engine on top of it
 * it is not chess games database

### What it might also be
Versions after 1 can include:
 * [PGN] (http://en.wikipedia.org/wiki/Portable_Game_Notation) load\save
 * [UCI] (http://en.wikipedia.org/wiki/Universal_Chess_Interface) interfaces
 * Chess game commenting facilities

# Known issues
 * you have to set up <assemblybinding> in your app.config [manually](http://stackoverflow.com/questions/30620085/add-bindingredirect-doesnt-change-app-config-though-it-should)
