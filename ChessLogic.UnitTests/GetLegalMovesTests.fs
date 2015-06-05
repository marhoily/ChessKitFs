module GetLegalMovesTests

open FsUnit.Xunit
open Xunit
open Parsing
open MoveLegalityChecker
open FenParser
open CoordinateNotation
open FenPrinter
open Definitions

let check from position = 
    position
    |> ParseFen
    |> unwrap
    |> GetLegalMoves.FromSquare (_c from)
    |> List.map CoordinateToString

[<Fact>]
let ``white pawn on h7``() = 
    "8/7P/8/8/8/8/8/8 w - - 0 1"
    |> check "h7"
    |> should equal ["g8"]           
