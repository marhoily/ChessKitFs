module AddObservationsTests

open FsUnit.Xunit
open Xunit
open ChessKit.ChessLogic
open ChessKit.ChessLogic.AddObservations
open ChessKit.ChessLogic.FenParser
open ChessKit.ChessLogic.Parsing
open ChessKit.ChessLogic.MoveLegalityChecker
open ChessKit.ChessLogic.CoordinateNotation
open ChessKit.ChessLogic.Text
open ChessKit.ChessLogic.San
open ChessKit.ChessLogic.FenPrinter
open ChessKit.ChessLogic.Extensions

let check expectedObservations position = 
    printfn "%s" (ToFen position)
    printfn "%s" (Dump position)
    position.Observations
    |> concatFieldNames " | "
    |> should equal expectedObservations

let checkObservations position move expectedObservations = 
    position
    |> ParseFen
    |> Operators.getSuccess
    |> ValidateLegalMove(_cn move)
    |> CoreToPosition
    |> check expectedObservations

let rec playFrom m p = 
    match m with
    | [] -> p
    | head :: tail -> 
        match p |> FromSanString head with
        | LegalSan(legal, _) -> playFrom tail (CoreToPosition legal)
        | x -> 
            printfn "%s" (ToFen p)
            printfn "%s" (Dump p)
            printfn "%s" head
            failwithf "%A" x

let playFromFen moves start = 
    start
    |> ParseFen
    |> Operators.getSuccess
    |> playFrom moves

let play moves = StartingPosition |> playFrom moves

[<Fact>]
let ``Gives check``() = 
    checkObservations "8/2Rk4/1q4BP/8/8/6K1/8/8 b - - 24 119" "b6-c7" "Check"

[<Fact>]
let ``Gives mate``() = 
    checkObservations "2K5/8/2k4r/8/8/8/8/8 b - - 0 9" "h6-h8" "Mate"

[<Fact>]
let ``Play should work``() = 
    let res = 
        play 
            [ "e4"; "e5"; "Nf3"; "Nc6"; "d4"; "exd4"; "Nxd4"; "Nf6"; "Nc3"; 
              "Bb4"; "Nxc6"; "bxc6"; "Bd3"; "d5"; "exd5"; "cxd5"; "O-O"; "O-O"; 
              "Bg5"; "Be6"; "Qf3"; "Be7"; "Rfe1"; "h6"; "Bxh6"; "gxh6"; "Rxe6"; 
              "fxe6"; "Qg3+"; "Kh8"; "Qg6" ]
    ToFen res 
    |> should equal "r2q1r1k/p1p1b3/4pnQp/3p4/8/2NB4/PPP2PPP/R5K1 b - - 3 16"

[<Fact>]
let ``PositionCore structural equality works``() = 
    let fen = "r2q1r1k/p1p1b3/4pnQp/3p4/8/2NB4/PPP2PPP/R5K1 b - - 3 16"
    let c1 = (ParseFen fen |> Operators.getSuccess).Core
    let c2 = (ParseFen fen |> Operators.getSuccess).Core
    c1 |> should equal c2
    c1.GetHashCode() |> should equal (c2.GetHashCode())
    [ c1; c2 ]
    |> Seq.countBy id
    |> List.ofSeq
    |> should equal [ (c1, 2) ]

[<Fact>]
let ``Draw by threefold repetition``() = 
    let res = 
        play 
            [ "e4"; "e5"; "Nf3"; "Nc6"; "d4"; "exd4"; "Nxd4"; "Nf6"; "Nc3"; 
              "Bb4"; "Nxc6"; "bxc6"; "Bd3"; "d5"; "exd5"; "cxd5"; "O-O"; "O-O"; 
              "Bg5"; "Be6"; "Qf3"; "Be7"; "Rfe1"; "h6"; "Bxh6"; "gxh6"; "Rxe6"; 
              "fxe6"; "Qg3+"; "Kh8"; "Qg6"; "Qe8"; "Qxh6+"; "Kg8"; "Qg5+"; "Kh8"; 
              "Qh6+"; "Kg8"; "Qg5+"; "Kh8"; "Qh6+" ]
    res |> check "Check | Repetition"

[<Fact>]
let ``50 moves rule clock increments after move``() = 
    let res = play [ "Nf3" ]
    res.HalfMoveClock |> should equal 1

[<Fact>]
let ``50 moves rule clock resets after pawn advance``() = 
    let res = play [ "Nf3"; "e5" ]
    res.HalfMoveClock |> should equal 0

[<Fact>]
let ``50 moves rule clock resets after pawn capture``() = 
    let res = play [ "e4"; "d5"; "Nf3"; "dxe4" ]
    res.HalfMoveClock |> should equal 0

[<Fact>]
let ``50 moves rule clock resets after capture``() = 
    let res = play [ "e4"; "d5"; "exd5"; "Qxd5" ]
    res.HalfMoveClock |> should equal 0

[<Fact>]
let ``Full moves clock does not increment after white's move``() = 
    let res = play [ "e4" ]
    res.FullMoveNumber |> should equal 1

[<Fact>]
let ``Full moves clock does increment after black's move``() = 
    "7K/5n2/4b3/8/8/8/7k/8 w - - 49 1"
    |> playFromFen [ "Kg7"; "Ng5" ]
    |> check "FiftyMoveRule"

[<Fact>]
let Stalemate() = 
    "7k/7P/8/7K/8/8/8/8 w - - 0 0"
    |> playFromFen [ "Kh6" ]
    |> check "Stalemate"

[<Fact>]
let ``count material``() = 
    (CountMaterial StartingPosition.Core |> sprintf "%A") 
    |> should equal 
           (([| 11; 2; 1; 1; 1 |], [| 11; 2; 1; 1; 1 |]) |> sprintf "%A")

[<Fact>]
let ``get square color``() = 
    GetSquareColor 0 |> should equal White
    GetSquareColor 1 |> should equal Black
    GetSquareColor 8 |> should equal Black
    GetSquareColor 63 |> should equal White

[<Fact>]
let ``insufficient material``() = 
    "7k/7P/8/7K/8/8/8/8 b - - 0 0"
    |> playFromFen [ "Kxh7" ]
    |> check "InsufficientMaterial"

[<Fact>]
let ``insufficient material: one knight``() = 
    "7k/8/8/7K/8/8/8/N7 b - - 0 0"
    |> playFromFen [ "Kh7" ]
    |> check "InsufficientMaterial"

[<Fact>]
let ``insufficient material: knight vs bishop``() = 
    "7k/8/8/7K/8/8/8/Nb6 b - - 0 0"
    |> playFromFen [ "Kh7" ]
    |> check "InsufficientMaterial"

[<Fact>]
let ``insufficient material: 3 white bishops``() = 
    "7k/8/8/7K/8/8/8/1B1B1B2 b - - 0 0"
    |> playFromFen [ "Kg7" ]
    |> check "InsufficientMaterial"
