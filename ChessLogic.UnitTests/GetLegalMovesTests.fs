module GetLegalMovesTests

open FsUnit.Xunit
open Xunit
open Parsing
open MoveLegalityChecker
open FenParser
open CoordinateNotation
open Definitions
open Dump

let check from (expected : string list) position = 
    let p = position |> ParseFen |> unwrap
    printf "%s" (Print p)
    p
    |> GetLegalMoves.FromSquare(_c from)
    |> List.map (fun m -> 
           match m.Move with
           | UsualMove(_, t) -> CoordinateToString t
           | PromotionMove({Vector = (_, t)}) -> CoordinateToString t)
    |> should equal expected

[<Fact>]
let ``empty square``() = 
    "8/8/8/8/8/8/8/8 w - - 0 1"
    |> check "e4" []

[<Fact>]
let ``white pawn on h6``() = 
    "8/8/7P/8/8/8/8/8 w - - 0 1"
    |> check "h6" [ "h7" ]

[<Fact>]
let ``white pawn on h7``() = 
    "8/7P/8/8/8/8/8/8 w - - 0 1"
    |> check "h7" [ "h8" ]

[<Fact>]
let ``white pawn on e2``() = 
    "8/8/8/8/8/8/4P3/8 w - - 0 1"
    |> check "e2" [ "e3"; "e4" ]

[<Fact>]
let ``e7-d8 capture``() = 
    "3qr3/4P3/8/8/8/8/8/8 w - - 0 1"
    |> check "e7" [ "d8" ]
