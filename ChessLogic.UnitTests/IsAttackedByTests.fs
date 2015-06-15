module IsAttackedByTests

open Xunit
open FsUnit.Xunit
open ChessKit.ChessLogic
open ChessKit.ChessLogic.IsAttackedBy
open ChessKit.ChessLogic.CoordinateNotation
open ChessKit.ChessLogic.X88

let check square func fen = 
    func (fun i -> 
          fen
          |> Fen.ParseFen
          |> Operators.getSuccess
          |> (fun x -> x.Core)
          |> PieceAt (i |> fromX88)) (toX88 (_c square))

[<Fact>]
let ``c6 is attacked by black pawn on b7``() = 
    "8/1p6/8/8/8/8/8/8 w - - 0 1"
    |> check "c6" (IsAttackedBy Black)
    |> should equal true

[<Fact>]
let ``a6 is attacked by black pawn on b7``() = 
    "8/1p6/8/8/8/8/8/8 w - - 0 1"
    |> check "a6" (IsAttackedBy Black)
    |> should equal true

[<Fact>]
let ``when check if a8 is attacked, it does not overflow``() = 
    "8/8/8/8/8/8/8/8 w - - 0 1"
    |> check "a8" (IsAttackedBy Black)
    |> should equal false

[<Fact>]
let ``h8 is attacked by black bishop on d4``() = 
    "8/8/8/8/3b4/8/8/8 w - - 0 1"
    |> check "h8" (IsAttackedBy Black)
    |> should equal true

[<Fact>]
let ``h8 is NOT attacked by black bishop on d4 because it's masked by the pawn on f6``() = 
    "8/8/5P2/8/3b4/8/8/8 w - - 0 1"
    |> check "h8" (IsAttackedBy Black)
    |> should equal false

[<Fact>]
let ``a7 is attacked by black bishop on d4``() = 
    "8/8/8/8/3b4/8/8/8 w - - 0 1"
    |> check "a7" (IsAttackedBy Black)
    |> should equal true

[<Fact>]
let ``a1 is attacked by black bishop on d4``() = 
    "8/8/8/8/3b4/8/8/8 w - - 0 1"
    |> check "a1" (IsAttackedBy Black)
    |> should equal true

[<Fact>]
let ``f2 is attacked by black bishop on d4``() = 
    "8/8/8/8/3b4/8/8/8 w - - 0 1"
    |> check "f2" (IsAttackedBy Black)
    |> should equal true

[<Fact>]
let ``c2 is attacked by black knight on d4``() = 
    "8/8/8/8/3n4/8/8/8 w - - 0 1"
    |> check "c2" (IsAttackedBy Black)
    |> should equal true

[<Fact>]
let ``b3 is attacked by black knight on d4``() = 
    "8/8/8/8/3n4/8/8/8 w - - 0 1"
    |> check "b3" (IsAttackedBy Black)
    |> should equal true


[<Fact>]
let ``b5 is attacked by black knight on d4``() = 
    "8/8/8/8/3n4/8/8/8 w - - 0 1"
    |> check "b5" (IsAttackedBy Black)
    |> should equal true


[<Fact>]
let ``c6 is attacked by black knight on d4``() = 
    "8/8/8/8/3n4/8/8/8 w - - 0 1"
    |> check "c6" (IsAttackedBy Black)
    |> should equal true


[<Fact>]
let ``e6 is attacked by black knight on d4``() = 
    "8/8/8/8/3n4/8/8/8 w - - 0 1"
    |> check "e6" (IsAttackedBy Black)
    |> should equal true

[<Fact>]
let ``f5 is attacked by black knight on d4``() = 
    "8/8/8/8/3n4/8/8/8 w - - 0 1"
    |> check "f5" (IsAttackedBy Black)
    |> should equal true

[<Fact>]
let ``f3 is attacked by black knight on d4``() = 
    "8/8/8/8/3n4/8/8/8 w - - 0 1"
    |> check "f3" (IsAttackedBy Black)
    |> should equal true

[<Fact>]
let ``e2 is attacked by black knight on d4``() = 
    "8/8/8/8/3n4/8/8/8 w - - 0 1"
    |> check "e2" (IsAttackedBy Black)
    |> should equal true

[<Fact>]
let ``d1 is attacked by black rook on d4``() = 
    "8/8/8/8/3r4/8/8/8 w - - 0 1"
    |> check "d1" (IsAttackedBy Black)
    |> should equal true

[<Fact>]
let ``d6 is attacked by black rook on d4``() = 
    "8/8/8/8/3r4/8/8/8 w - - 0 1"
    |> check "d6" (IsAttackedBy Black)
    |> should equal true

[<Fact>]
let ``f4 is attacked by black rook on d4``() = 
    "8/8/8/8/3r4/8/8/8 w - - 0 1"
    |> check "f4" (IsAttackedBy Black)
    |> should equal true

[<Fact>]
let ``a4 is attacked by black rook on d4``() = 
    "8/8/8/8/3r4/8/8/8 w - - 0 1"
    |> check "a4" (IsAttackedBy Black)
    |> should equal true

[<Fact>]
let ``c4 is attacked by black queen on d4``() = 
    "8/8/8/8/3q4/8/8/8 w - - 0 1"
    |> check "c4" (IsAttackedBy Black)
    |> should equal true

[<Fact>]
let ``c3 is attacked by black queen on d4``() = 
    "8/8/8/8/3q4/8/8/8 w - - 0 1"
    |> check "c3" (IsAttackedBy Black)
    |> should equal true

[<Fact>]
let ``d3 is attacked by black queen on d4``() = 
    "8/8/8/8/3q4/8/8/8 w - - 0 1"
    |> check "d3" (IsAttackedBy Black)
    |> should equal true

[<Fact>]
let ``e3 is attacked by black queen on d4``() = 
    "8/8/8/8/3q4/8/8/8 w - - 0 1"
    |> check "e3" (IsAttackedBy Black)
    |> should equal true

[<Fact>]
let ``e4 is attacked by black queen on d4``() = 
    "8/8/8/8/3q4/8/8/8 w - - 0 1"
    |> check "e4" (IsAttackedBy Black)
    |> should equal true

[<Fact>]
let ``e5 is attacked by black queen on d4``() = 
    "8/8/8/8/3q4/8/8/8 w - - 0 1"
    |> check "e5" (IsAttackedBy Black)
    |> should equal true

[<Fact>]
let ``d5 is attacked by black queen on d4``() = 
    "8/8/8/8/3q4/8/8/8 w - - 0 1"
    |> check "d5" (IsAttackedBy Black)
    |> should equal true

[<Fact>]
let ``c5 is attacked by black queen on d4``() = 
    "8/8/8/8/3q4/8/8/8 w - - 0 1"
    |> check "c5" (IsAttackedBy Black)
    |> should equal true

// =============== King ================

[<Fact>]
let ``c4 is attacked by black king on d4``() = 
    "8/8/8/8/3k4/8/8/8 w - - 0 1"
    |> check "c4" (IsAttackedBy Black)
    |> should equal true

[<Fact>]
let ``c3 is attacked by black king on d4``() = 
    "8/8/8/8/3k4/8/8/8 w - - 0 1"
    |> check "c3" (IsAttackedBy Black)
    |> should equal true

[<Fact>]
let ``d3 is attacked by black king on d4``() = 
    "8/8/8/8/3k4/8/8/8 w - - 0 1"
    |> check "d3" (IsAttackedBy Black)
    |> should equal true

[<Fact>]
let ``e3 is attacked by black king on d4``() = 
    "8/8/8/8/3k4/8/8/8 w - - 0 1"
    |> check "e3" (IsAttackedBy Black)
    |> should equal true

[<Fact>]
let ``e4 is attacked by black king on d4``() = 
    "8/8/8/8/3k4/8/8/8 w - - 0 1"
    |> check "e4" (IsAttackedBy Black)
    |> should equal true

[<Fact>]
let ``e5 is attacked by black king on d4``() = 
    "8/8/8/8/3k4/8/8/8 w - - 0 1"
    |> check "e5" (IsAttackedBy Black)
    |> should equal true

[<Fact>]
let ``d5 is attacked by black king on d4``() = 
    "8/8/8/8/3k4/8/8/8 w - - 0 1"
    |> check "d5" (IsAttackedBy Black)
    |> should equal true

[<Fact>]
let ``c5 is attacked by black king on d4``() = 
    "8/8/8/8/3k4/8/8/8 w - - 0 1"
    |> check "c5" (IsAttackedBy Black)
    |> should equal true

// =============== White ===============

[<Fact>]
let ``c8 is attacked by white pawn on b7``() = 
    "8/1P6/8/8/8/8/8/8 w - - 0 1"
    |> check "c8" (IsAttackedBy White)
    |> should equal true

[<Fact>]
let ``a8 is attacked by white pawn on b7``() = 
    "8/1P6/8/8/8/8/8/8 w - - 0 1"
    |> check "a8" (IsAttackedBy White)
    |> should equal true

// =============== IsInCheck ===============

let check2 func fen = 
    func (fun i -> 
          fen
          |> Fen.ParseFen
          |> Operators.getSuccess
          |> (fun x -> x.Core)
          |> PieceAt i) 

[<Fact>]
let ``black is in check when their king on c8 is attacked by pawn on b7``() = 
    "2k5/1P6/8/8/8/8/8/8 w - - 0 1"
    |> check2 (IsInCheck Black)
    |> should equal true

[<Fact>]
let ``black is not in check when there is no white pieces on the board``() = 
    "2k5/8/8/8/8/8/8/8 w - - 0 1"
    |> check2 (IsInCheck Black)
    |> should equal false

