module DumpTests

open ChessKit.ChessLogic.FenParser;
open ChessKit.ChessLogic.Parsing;
open ChessKit.ChessLogic;
open ChessKit.ChessLogic.Text;
open Xunit;
open FsUnit.Xunit

[<Fact>]
let printOut() =
    "1r2k2r/p2n1p1p/np4p1/2p1B1b1/7P/1P1P4/P1PN3P/RNQ2RK1 b k - 0 18"
    |> ParseFen
    |> unwrap
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
