module GetLegalMovesTests

open FsUnit.Xunit
open Xunit
open Parsing
open MoveLegalityChecker
open FenParser
open CoordinateNotation
open Definitions
open Dump

let check from position = 
    let p = position |> ParseFen |> unwrap
    printf "%s" (Print p)
    p
    |> GetLegalMoves.FromSquare(_c from)
    |> List.map (fun m -> 
           match m.Move with
           | UsualMove(_, t) -> CoordinateToString t
           | PromotionMove({Vector = (_, t)}) -> CoordinateToString t)

[<Fact>]
let ``white pawn on h6``() = 
    "8/8/7P/8/8/8/8/8 w - - 0 1"
    |> check "h6"
    |> should equal [ "h7" ]

[<Fact>]
let ``white pawn on h7``() = 
    "8/7P/8/8/8/8/8/8 w - - 0 1"
    |> check "h7"
    |> should equal [ "h8" ]
