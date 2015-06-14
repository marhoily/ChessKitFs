module AddObservationsTests

open AddObservations
open FenParser
open Parsing
open MoveLegalityChecker
open FsUnit.Xunit
open CoordinateNotation
open Definitions
open Xunit
open San
open FenPrinter

let observationsToString (pos : Position) = 
    let strings = pos.Observations |> List.map toString
    String.concat " | " strings

let checkObservations position move expectedObservations = 
    position
    |> ParseFen
    |> unwrap
    |> ValidateLegalMove(_cn move)
    |> CoreToPosition
    |> observationsToString
    |> should equal expectedObservations

let rec playFrom m p = 
    match m with
    | [] -> p
    | head :: tail -> 
        match p |> FromSanString head with
        | LegalSan(legal, _) -> playFrom tail (CoreToPosition legal)
        | x -> 
            printfn "%s" (ToFen p)
            printfn "%s" (Dump.Print p)
            printfn "%s" head
            failwithf "%A" x

let playFen start moves = 
    start
    |> ParseFen
    |> unwrap
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
    let c1 = (ParseFen fen |> unwrap).Core
    let c2 = (ParseFen fen |> unwrap).Core
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
    let strings = res.Observations |> List.map toString
    let actual = String.concat ", " strings
    actual |> should equal "Repetition, Check"

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
    let res = play [ "e4"; "e5" ]
    res.FullMoveNumber |> should equal 2
