module GetLegalMovesTests

open FsUnit.Xunit
open Xunit
open ChessKit.ChessLogic
open ChessKit.ChessLogic.MoveLegalityChecker
open ChessKit.ChessLogic.Fen
open ChessKit.ChessLogic.CoordinateNotation
open ChessKit.ChessLogic.Text
open ChessKit.ChessLogic.Extensions

let toString (m : LegalMove) = squareToString m.Move.End

let check from (expected : string list) position = 
    let p = Operators.getSuccess (position |> ParseFen)
    printf "%s" (Dump p)
    let f = ParseCoordinate from
    
    let actual = 
        p
        |> GetLegalMoves.FromSquare f
        |> List.map toString
        |> List.sort
    actual |> should equal (expected |> List.sort)
    // Now do full search and make sure ValidateMove agrees
    let expected2 = 
        [ for i = 0 to 63 do
              let e = (i % 8, i / 8)
              let t = Move.Create f e None
              match ValidateMove t p with
              | LegalMove _ -> yield squareToString e
              | _ -> () ]
    actual |> should equal (expected2 |> List.sort)

let checkAll expected position = 
    let p = Operators.getSuccess (position |> ParseFen)
    printf "%s" (Dump p)
    let actual = 
        p
        |> GetLegalMoves.All
        |> List.map toString
        |> List.sort
    actual |> should equal (expected |> List.sort)
    // Now do full search and make sure ValidateMove agrees
    let expected2 = 
        [ for i = 0 to 63 do
              for j = 0 to 63 do
                  let e = (i % 8, i / 8)
                  let t = Move.Create (j % 8, j / 8) e None
                  match ValidateMove t p with
                  | LegalMove _ -> yield squareToString e
                  | _ -> () ]
    actual |> should equal (expected2 |> List.sort)

[<Fact>]
let ``empty square``() = "8/8/8/8/8/8/8/8 w - - 0 1" |> check "e4" []

[<Fact>]
let rook() = 
    "8/2N5/8/8/8/2R2q2/8/8 w - - 0 1" 
    |> check "c3" [ "a3"; "b3"; "c1"; "c2"; "c4"; "c5"; "c6"; "d3"; "e3"; "f3" ]

[<Fact>]
let queen() = 
    "8/8/2n5/4P3/8/2q2n2/8/8 b - - 0 1" 
    |> check "c3" 
           [ "a3"; "b3"; "c1"; "c2"; "c4"; "c5"; "d3"; "e3"; "a1"; "b2"; "d4"; 
             "e5"; "a5"; "b4"; "d2"; "e1" ]

[<Fact>]
let king() = 
    "8/8/8/8/4K3/8/8/8 w - - 0 1" 
    |> check "e4" [ "d3"; "d4"; "d5"; "e3"; "e5"; "f3"; "f4"; "f5" ]

[<Fact>]
let knight() = 
    "8/8/8/8/4n3/8/8/8 b - - 0 1" 
    |> check "e4" [ "d2"; "c3"; "c5"; "d6"; "f6"; "g5"; "g3"; "f2" ]

[<Fact>]
let all() = "8/8/8/3pp3/3PP3/8/8/8 w - - 0 2" |> checkAll [ "e5"; "d5" ]

// ---------------- White Pawn ----------------
[<Fact>]
let ``white pawn: h6-h7``() = 
    "8/8/7P/8/8/8/8/8 w - - 0 1" |> check "h6" [ "h7" ]

[<Fact>]
let ``white pawn: h7-h8=Q``() = 
    "8/7P/8/8/8/8/8/8 w - - 0 1" |> check "h7" [ "h8" ]

[<Fact>]
let ``white pawn: e2-e3, e2-e4``() = 
    "8/8/8/8/8/8/4P3/8 w - - 0 1" |> check "e2" [ "e3"; "e4" ]

[<Fact>]
let ``white pawn: e7-d8=Q capture``() = 
    "3qr3/4P3/8/8/8/8/8/8 w - - 0 1" |> check "e7" [ "d8" ]

[<Fact>]
let ``white pawn: e7-f8=Q capture``() = 
    "4rr2/4P3/8/8/8/8/8/8 w - - 0 1" |> check "e7" [ "f8" ]

[<Fact>]
let ``white pawn: e2-d3 capture``() = 
    "8/8/8/8/8/3qr3/4P3/8 w - - 0 1" |> check "e2" [ "d3" ]

[<Fact>]
let ``white pawn: e2-f3 capture``() = 
    "8/8/8/8/8/4rr2/4P3/8 w - - 0 1" |> check "e2" [ "f3" ]

// ---------------- Black Pawn ----------------
[<Fact>]
let ``black pawn: h3-h2``() = 
    "8/8/8/8/8/7p/8/8 b - - 0 1" |> check "h3" [ "h2" ]

[<Fact>]
let ``black pawn: h2-h1=Q``() = 
    "8/8/8/8/8/8/7p/8 b - - 0 1" |> check "h2" [ "h1" ]

[<Fact>]
let ``black pawn: e7-e6, e7-e5``() = 
    "8/4p3/8/8/8/8/8/8 b - - 0 1" |> check "e7" [ "e6"; "e5" ]

[<Fact>]
let ``black pawn: e2-d1=Q capture``() = 
    "8/8/8/8/8/8/4p3/3RB3 b - - 0 1" |> check "e2" [ "d1" ]

[<Fact>]
let ``black pawn: e2-f1=Q capture``() = 
    "8/8/8/8/8/8/4p3/4NQ2 b - - 0 1" |> check "e2" [ "f1" ]

[<Fact>]
let ``black pawn: e7-d6 capture``() = 
    "8/4p3/3BN3/8/8/8/8/8 b - - 0 1" |> check "e7" [ "d6" ]

[<Fact>]
let ``black pawn: e7-f6 capture``() = 
    "8/4p3/4RR2/8/8/8/8/8 b - - 0 1" |> check "e7" [ "f6" ]

// ---------------- Bishop ----------------
[<Fact>]
let ``bishop: a1-b2``() = "8/8/8/8/8/2P5/8/B7 w - - 0 1" |> check "a1" [ "b2" ]

[<Fact>]
let ``bishop: a8-b7``() = "b7/8/2p5/8/8/8/8/8 b - - 0 1" |> check "a8" [ "b7" ]

[<Fact>]
let ``bishop: h8-g7``() = "7b/8/5p2/8/8/8/8/8 b - - 0 1" |> check "h8" [ "g7" ]

[<Fact>]
let ``bishop: h1-g2``() = "8/8/8/8/8/5P2/8/7B w - - 0 1" |> check "h1" [ "g2" ]

[<Fact>]
let ``bishop: h1-f3``() = 
    "8/8/8/8/4p3/8/8/7b b - - 0 1" |> check "h1" [ "g2"; "f3" ]

[<Fact>]
let ``bishop: g1-h2, g1-f2``() = 
    "8/8/8/8/8/4p3/8/6b1 b - - 0 1" |> check "g1" [ "h2"; "f2" ]

[<Fact>]
let ``bishop captures: h1-g2``() = 
    "8/8/8/8/8/8/6B1/7b b - - 0 1" |> check "h1" [ "g2" ]
