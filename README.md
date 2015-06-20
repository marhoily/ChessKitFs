# Chess Kit (F#)
[![Build status](https://ci.appveyor.com/api/projects/status/20euy0r0xxsp19fx?svg=true)](https://ci.appveyor.com/project/marhoily/chesskitfs)
[![ChessKit.ChessLogic](https://img.shields.io/nuget/v/ChessKit.ChessLogic.svg)](https://www.nuget.org/packages/ChessKit.ChessLogic/)
[![ChessKit.ChessLogic](https://img.shields.io/nuget/vpre/ChessKit.ChessLogic.svg)](https://www.nuget.org/packages/ChessKit.ChessLogic/)

### What it is
Chess Kit [includes](https://trello.com/b/80MHIZWN/chess-f):
 * Immutable Board representation
 * Parsing moves in algebraic notation ([SAN](http://en.wikipedia.org/wiki/Algebraic_notation_(chess)))
 * Detect position properties: check, mate, stalemate, draw by repetition, 50 moves rule, insufficient material.
```csharp
// Fool's Mate
var position = Board.StartingPosition
    .MakeMove("f3")
    .MakeMove("e5")
    .MakeMove("g4")
    .MakeMove("Qh4#");
Console.WriteLine(position.Dump());
Console.WriteLine(position.Properties);
```
```
prints out:
 ╔═══╤═══╤═══╤═══╤═══╤═══╤═══╤═══╗
8║ r │ n │ b │   │ k │ b │ n │ r ║
 ╟───┼───┼───┼───┼───┼───┼───┼───╢
7║ p │ p │ p │ p │   │ p │ p │ p ║
 ╟───┼───┼───┼───┼───┼───┼───┼───╢
6║   │   │   │   │   │   │   │   ║
 ╟───┼───┼───┼───┼───┼───┼───┼───╢
5║   │   │   │   │ p │   │   │   ║
 ╟───┼───┼───┼───┼───┼───┼───┼───╢
4║   │   │   │   │   │   │ P │ q ║
 ╟───┼───┼───┼───┼───┼───┼───┼───╢
3║   │   │   │   │   │ P │   │   ║
 ╟───┼───┼───┼───┼───┼───┼───┼───╢
2║ P │ P │ P │ P │ P │   │   │ P ║
 ╟───┼───┼───┼───┼───┼───┼───┼───╢
1║ R │ N │ B │ Q │ K │ B │ N │ R ║
 ╚═══╧═══╧═══╧═══╧═══╧═══╧═══╧═══╝
   A   B   C   D   E   F   G   H

Mate
```
 * Parse [FEN](http://en.wikipedia.org/wiki/Forsyth%E2%80%93Edwards_Notation)
 * Parse moves in [coordinate](http://en.wikipedia.org/wiki/Chess_notation) notation
 * Print FEN
```csharp
var move = Move.Parse("b7-b8=Q");
var legalMove = Fen
    .Parse("8/1P6/8/8/8/8/8/8 w - - 0 1")
    .ValidateLegalMove(move);
var nextPosition = legalMove.ToPosition();
Console.WriteLine(Fen.Print(nextPosition));
```
```
prints out:
1Q6/8/8/8/8/8/8/8 b - - 0 1
```

 * Apply the moves to the board and not only get a new board, but also all possible info about that move, like why it is illegal
 * Get all legal moves for a piece. This is not based on trying to apply moves from all squares to all squaresn, but a dedicated algorithm
 * Print the board nicely as text ;)
 
### What it is not 
 * it is not a chess engine
 * it's not nearly optimal enough to enable you to write a chess engine on top of it
 * it is not chess games database

### What it might also be
Versions after 1 can include:
 * [Zobrist hash key](http://en.wikipedia.org/wiki/Zobrist_hashing) for a position
 * [PGN] (http://en.wikipedia.org/wiki/Portable_Game_Notation) load\save
 * [UCI] (http://en.wikipedia.org/wiki/UniversalMove.Parsehess_Interface) interfaces
 * Chess game commenting facilities
 
### Known issues
 * you have to set up <assemblybinding> in your app.config [manually](http://stackoverflow.com/questions/30620085/add-bindingredirect-doesnt-change-app-config-though-it-should)