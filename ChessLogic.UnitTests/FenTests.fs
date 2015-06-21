module FenTests

open Xunit
open FsUnit.Xunit
open ChessKit.ChessLogic
open ChessKit.ChessLogic.PositionCoreExt
open ChessKit.ChessLogic.BoardTextExtensions
open TestUtils


let toFenCore c = 
    Fen.Print { EmptyPosition with Core = c } 

let ec = EmptyPosition.Core;

[<Fact>]
let ``ToFen works``() = 
    toFenCore { ec with Placement = [| Piece.WhitePawn |] } 
    |> should equal "P w KQkq - 0 1"

[<Fact>]
let ``ToFen works with one empty square``() = 
    toFenCore { ec with Placement = 
                                   [| Piece.None
                                      Piece.WhitePawn |] }
    |> should equal "1P w KQkq - 0 1"

[<Fact>]
let ``ToFen works with two empty squares``() = 
    toFenCore { ec with Placement = 
                                   [| Piece.None
                                      Piece.None
                                      Piece.WhitePawn |] }
    |> should equal "2P w KQkq - 0 1"

[<Fact>]
let ``ToFen works with empty squares in the middle``() = 
    toFenCore { ec with Placement = 
                                   [| Piece.WhitePawn
                                      Piece.None
                                      Piece.None
                                      Piece.BlackBishop |] }
    |> should equal "P2b w KQkq - 0 1"

[<Fact>]
let ``ToFen works with 2 ranks``() = 
    toFenCore { ec with Placement = 
                                   [| Piece.WhitePawn
                                      Piece.None
                                      Piece.None
                                      Piece.None
                                      Piece.None
                                      Piece.None
                                      Piece.None
                                      Piece.None
                                      Piece.BlackBishop |] }
    |> should equal "P7/b w KQkq - 0 1"

[<Fact>]
let ``ToFen works with piece on the H file``() = 
    toFenCore { ec with Placement = 
                                   [| Piece.WhitePawn
                                      Piece.None
                                      Piece.None
                                      Piece.None
                                      Piece.None
                                      Piece.None
                                      Piece.None
                                      Piece.BlackBishop |] }
    |> should equal "P6b w KQkq - 0 1"

[<Fact>]
let ``ToFen works with empty ranks``() = 
    toFenCore { ec with Placement = 
                                   [| Piece.None
                                      Piece.None
                                      Piece.None
                                      Piece.None
                                      Piece.None
                                      Piece.None
                                      Piece.None
                                      Piece.None
                                      Piece.BlackBishop |] }
    |> should equal "8/b w KQkq - 0 1"

[<Fact>]
let ``ToFen works with empty rank after non-empty rank``() = 
    toFenCore { ec with Placement = 
                                   [| Piece.WhitePawn
                                      Piece.None
                                      Piece.None
                                      Piece.None
                                      Piece.None
                                      Piece.None
                                      Piece.None
                                      Piece.None
                                      Piece.None
                                      Piece.None
                                      Piece.None
                                      Piece.None
                                      Piece.None
                                      Piece.None
                                      Piece.None
                                      Piece.None
                                      Piece.BlackBishop |] }
    |> should equal "P7/8/b w KQkq - 0 1"

[<Fact>]
let ``ToFen works with round number of squares``() = 
    toFenCore { ec with Placement = 
                                   [| Piece.WhitePawn
                                      Piece.None
                                      Piece.None
                                      Piece.None
                                      Piece.None
                                      Piece.None
                                      Piece.None
                                      Piece.None |] }
    |> should equal "P7 w KQkq - 0 1"

[<Fact>]
let ``ToFen prints en-passant for Color.White correctly``() = 
    toFenCore { ec with EnPassant = Some(4) } 
    |> should equal " w KQkq e6 0 1"

[<Fact>]
let ``ToFen prints en-passant for black correctly``() = 
    toFenCore { ec with EnPassant = Some(4)
                        ActiveColor = Color.Black }
    |> should equal " b KQkq e3 0 1"

[<Fact>]
let ``ToFen prints castling availability correctly when some unavailable``() = 
    toFenCore { ec with CastlingAvailability = Castlings.None } 
    |> should equal " w - - 0 1"

let positive = ParseToStringShouldMatch Fen.Print Fen.Parse
let negative = ErrorMessageShouldMatch Fen.TryParse

[<Fact>]
let works() = positive "p2P3n/8 b KQ - 0 1"

[<Fact>]
let ``works with en-passant``() = positive "p2P3n/8 w KQ e6 0 1"

[<Fact>]
let ``works with no castlings available``() = positive "p2P3n/8 w - e6 0 1"

[<Fact>]
let ``works with starting position``() = 
    positive "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"

[<Fact>]
let ``works with en-passant for black``() = 
    positive "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1"

[<Fact>]
let ``fails nicely with invalid input``() = negative "x w - e3 0 1" "Error in Ln: 1 Col: 1
x w - e3 0 1
^
Expecting: number 1..8 or piece symbol
"

[<Fact>]
let ``starting position print-out looks fine``() = 
    Board.StartingPosition
    |> Dump
    |> should equal (
          " ╔═══╤═══╤═══╤═══╤═══╤═══╤═══╤═══╗\r\n" +
          "8║ r │ n │ b │ q │ k │ b │ n │ r ║\r\n" +
          " ╟───┼───┼───┼───┼───┼───┼───┼───╢\r\n" +
          "7║ p │ p │ p │ p │ p │ p │ p │ p ║\r\n" +
          " ╟───┼───┼───┼───┼───┼───┼───┼───╢\r\n" +
          "6║   │   │   │   │   │   │   │   ║\r\n" +
          " ╟───┼───┼───┼───┼───┼───┼───┼───╢\r\n" +
          "5║   │   │   │   │   │   │   │   ║\r\n" +
          " ╟───┼───┼───┼───┼───┼───┼───┼───╢\r\n" +
          "4║   │   │   │   │   │   │   │   ║\r\n" +
          " ╟───┼───┼───┼───┼───┼───┼───┼───╢\r\n" +
          "3║   │   │   │   │   │   │   │   ║\r\n" +
          " ╟───┼───┼───┼───┼───┼───┼───┼───╢\r\n" +
          "2║ P │ P │ P │ P │ P │ P │ P │ P ║\r\n" +
          " ╟───┼───┼───┼───┼───┼───┼───┼───╢\r\n" +
          "1║ R │ N │ B │ Q │ K │ B │ N │ R ║\r\n" +
          " ╚═══╧═══╧═══╧═══╧═══╧═══╧═══╧═══╝\r\n" +
          "   A   B   C   D   E   F   G   H  \r\n")

[<Fact>]
let ``d1 should refer to Q in starting position``() = 
    Board.StartingPosition.Core.atStr "d1"
    |> Text.pieceToChar
    |> should equal 'Q'