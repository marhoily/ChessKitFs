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
    let toString m =
        match m.Move with
        | UsualMove(_, t) -> CoordinateToString t
        | PromotionMove({Vector = (_, t)}) -> CoordinateToString t

    let p = position |> ParseFen |> unwrap
    printf "%s" (Print p)
    let f = _c from
    let actual = 
        p
        |> GetLegalMoves.FromSquare f
        |> List.map toString
        |> List.sort
    actual |> should equal (expected |> List.sort)

    let expected2 = 
        [ for i = 0 to 63 do
            let t = UsualMove(f, (i%8, i/8))
            let m = ValidateMove t p
            let valid = 
                m.Hint.Errors 
                |> List.filter (fun err -> err <> MissingPromotionHint)
                |> List.isEmpty
            if valid then yield m |> toString ]

    actual |> should equal (expected2 |> List.sort)

[<Fact>]
let ``empty square``() = 
    "8/8/8/8/8/8/8/8 w - - 0 1"
    |> check "e4" []

// ---------------- White Pawn ----------------
[<Fact>]
let ``white pawn on h6``() = 
    "8/8/7P/8/8/8/8/8 w - - 0 1"
    |> check "h6" [ "h7" ]

[<Fact>]
let ``white pawn on h7``() = 
    "8/7P/8/8/8/8/8/8 w - - 0 1"
    |> check "h7" [ "h8" ]

[<Fact>]
let ``e2-e3, e2-e4``() = 
    "8/8/8/8/8/8/4P3/8 w - - 0 1"
    |> check "e2" [ "e3"; "e4" ]

[<Fact>]
let ``e7-d8 capture``() = 
    "3qr3/4P3/8/8/8/8/8/8 w - - 0 1"
    |> check "e7" [ "d8" ]

[<Fact>]
let ``e7-f8 capture``() = 
    "4rr2/4P3/8/8/8/8/8/8 w - - 0 1"
    |> check "e7" [ "f8" ]

[<Fact>]
let ``e2-d3 capture``() = 
    "8/8/8/8/8/3qr3/4P3/8 w - - 0 1"
    |> check "e2" [ "d3" ]

[<Fact>]
let ``e2-f3 capture``() = 
    "8/8/8/8/8/4rr2/4P3/8 w - - 0 1"
    |> check "e2" [ "f3" ]