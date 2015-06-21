module DefinitionsTests

open Xunit
open FsUnit.Xunit
open TestUtils
open ChessKit.ChessLogic

open ChessKit.ChessLogic
open ChessKit.ChessLogic.BoardTextExtensions

[<Fact>]
let printOut() =
    "1r2k2r/p2n1p1p/np4p1/2p1B1b1/7P/1P1P4/P1PN3P/RNQ2RK1 b k - 0 18"
    |> Fen.Parse
    |> Dump
    |> should equal (
          " ╔═══╤═══╤═══╤═══╤═══╤═══╤═══╤═══╗\r\n" +
          "8║   │ r │   │   │ k │   │   │ r ║\r\n" +
          " ╟───┼───┼───┼───┼───┼───┼───┼───╢\r\n" +
          "7║ p │   │   │ n │   │ p │   │ p ║\r\n" +
          " ╟───┼───┼───┼───┼───┼───┼───┼───╢\r\n" +
          "6║ n │ p │   │   │   │   │ p │   ║\r\n" +
          " ╟───┼───┼───┼───┼───┼───┼───┼───╢\r\n" +
          "5║   │   │ p │   │ B │   │ b │   ║\r\n" +
          " ╟───┼───┼───┼───┼───┼───┼───┼───╢\r\n" +
          "4║   │   │   │   │   │   │   │ P ║\r\n" +
          " ╟───┼───┼───┼───┼───┼───┼───┼───╢\r\n" +
          "3║   │ P │   │ P │   │   │   │   ║\r\n" +
          " ╟───┼───┼───┼───┼───┼───┼───┼───╢\r\n" +
          "2║ P │   │ P │ N │   │   │   │ P ║\r\n" +
          " ╟───┼───┼───┼───┼───┼───┼───┼───╢\r\n" +
          "1║ R │ N │ Q │   │   │ R │ K │   ║\r\n" +
          " ╚═══╧═══╧═══╧═══╧═══╧═══╧═══╧═══╝\r\n" +
          "   A   B   C   D   E   F   G   H  \r\n")

let positive = 
    ParseToStringShouldMatch Move.toString Move.Parse
let negative = ErrorMessageShouldMatch Move.TryParse

[<Fact>]
let ``(4,6) -> (4,4) should read "e2-e4"``() = 
    Move.Create (4, 6) (4, 4) PieceType.None
    |> Move.toString
    |> should equal "e2-e4"

[<Fact>]
let ``Promotion move should read correctly``() = 
    Move.Create (4, 6) (4, 4) PieceType.Queen
    |> Move.toString
    |> should equal "e2-e4=Q"

[<Fact>]
let ``works with e2-e4``() = positive "e2-e4"

[<Fact>]
let ``works with a1-h8``() = positive "a1-h8"

[<Fact>]
let ``works with e2-e4=Q``() = positive "e2-e4=Q"

[<Fact>]
let ``works with a1-h8=Q``() = positive "a1-h8=Q"

[<Fact>]
let ``meaningful error for a1h8``() = negative "a1h8" "Error in Ln: 1 Col: 3
a1h8
  ^
Expecting: '-' or 'x'
"
