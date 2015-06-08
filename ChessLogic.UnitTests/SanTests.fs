module SanTests

open FsUnit.Xunit
open Xunit
open Parsing
open MoveLegalityChecker
open FenParser
open CoordinateNotation
open San
open Definitions

// ----- ToSanString --------
let check move expectedSan position = 
    let p = 
        position
        |> ParseFen
        |> unwrap
    printfn "%s" (Dump.Print p)
    p
    |> ValidateLegalMove(_cn move)
    |> ToSanString
    |> should equal expectedSan

[<Fact>]
let ``O-O``() = 
    "r3kb1r/pppb1pp1/8/7p/3PPBn1/2N5/PPP3PP/R2QK2R w KQkq - 0 14" 
    |> check "e1-g1" "O-O"

[<Fact>]
let ``O-O-O``() = 
    "r3kn1r/pp4pp/1q2b3/n2pPpP1/2pP4/2P1BB2/PP4QP/RN3RK1 b kq - 0 16" 
    |> check "e8-c8" "O-O-O"

[<Fact>]
let ``Pawn capture``() = 
    "r1b1kn1r/pp4pp/1q2p3/n2pPpP1/2pP2P1/2P1B3/PP1QB2P/RN3RK1 w kq - 0 14" 
    |> check "g4-f5" "gxf5"

[<Fact>]
let ``Pawn push``() = 
    "r1b1kb1r/pp1n1ppp/1q2p3/n2pP3/2pP1P2/2P1BN2/PP1QB1PP/RN3RK1 b kq - 0 10" 
    |> check "f7-f5" "f5"

[<Fact>]
let Knight() = 
    "r1b1kb1r/pp1n2pp/1q2p3/n2pPp2/2pP1P2/2P1BN2/PP1QB1PP/RN3RK1 w kq f6 0 11" 
    |> check "f3-g5" "Ng5"

[<Fact>]
let Check() = 
    "2kr4/pp2n1p1/6qr/n3P3/1NpP1p2/2P2B2/PQ3B1P/Rb3K2 b - - 0 28" 
    |> check "b1-d3" "Bd3+"

[<Fact>]
let Promotion() = 
    "5r2/ppkP2p1/4P3/n2n1q2/1Np2p2/2P2Q2/P4B1r/R3K3 w - - 0 36" 
    |> check "d7-d8=N" "d8=N"

[<Fact>]
let Nec6() = 
    "2kr4/pp2n1p1/6qr/n3P3/1NpP1p2/2P2B2/PQ3B1P/Rb3K2 b - - 0 28" 
    |> check "e7-c6" "Nec6"

[<Fact>]
let N7c6() = 
    "2kr4/pp2n1p1/6qr/4n3/2pP1p2/2PN1B2/PQ3B1P/R4K2 b - - 0 29" 
    |> check "e7-c6" "N7c6"

[<Fact>]
let Ne5c6() = 
    "2kr4/pp2n1p1/6qr/n3n3/2pP1p2/2PN1B2/PQ3B1P/R4K2 b - - 0 29" 
    |> check "e5-c6" "Ne5c6"

// ----- ParseSanString --------
let parse str expected = 
    ParseSanString str
    |> wrap
    |> unwrap
    |> sprintf "%A"
    |> should equal expected

[<Fact>]
let ``parse O-O-O``() = parse "O-O-O" "(LongCastling, null)"

[<Fact>]
let ``parse O-O``() = parse "O-O" "(ShortCastling, null)"

[<Fact>]
let ``parse Bg5``() = 
    parse "Bg5" "(Usual (Bishop, (NoHint, (null, (6, 3)))), null)"

[<Fact>]
let ``parse Nec6``() = 
    parse "Nec6" "(Usual (Knight, (FileHint 4, (null, (2, 2)))), null)"

[<Fact>]
let ``parse K5c6``() = 
    parse "N5c6" "(Usual (Knight, (RankHint 3, (null, (2, 2)))), null)"

[<Fact>]
let ``parse Qe1f8``() = 
    parse "Qe1f8" "(Usual (Queen, (SquareHint (4, 7), (null, (5, 0)))), null)"

[<Fact>]
let ``parse Ne:c6``() = 
    parse "Ne:c6" 
        "(Usual (Knight, (FileHint 4, (Some SanCapture, (2, 2)))), null)"

[<Fact>]
let ``parse K5:c6``() = 
    parse "N5:c6" 
        "(Usual (Knight, (RankHint 3, (Some SanCapture, (2, 2)))), null)"

[<Fact>]
let ``parse Qe1xf8``() = 
    parse "Qe1xf8" 
        "(Usual (Queen, (SquareHint (4, 7), (Some SanCapture, (5, 0)))), null)"

[<Fact>]
let ``parse e4``() = parse "e4" "(PawnPush ((4, 4),null), null)"

[<Fact>]
let ``parse f8=Q``() = parse "f8=Q" "(PawnPush ((5, 0),Some Queen), null)"

[<Fact>]
let ``parse c1=N+``() = 
    parse "c1=N+" "(PawnPush ((2, 7),Some Knight), Some SanCheck)"

[<Fact>]
let ``parse a2#``() = parse "a2#" "(PawnPush ((0, 6),null), Some SanMate)"

[<Fact>]
let ``parse gxe4``() = parse "gxe4" "(PawnCapture (6,((4, 4), null)), null)"

// ----- Scanners --------
let findPushingPawns square (expected : string list) board = 
    let scan, _, _ = sanScanners (ParseFen board |> unwrap)
    scan (_c square |> toX88)
    |> List.map CoordinateToString
    |> should equal expected

[<Fact>]
let ``push black pawn: b6-b5``() = 
    "8/1p6/1p6/8/8/8/8/8 b - - 0 1" |> findPushingPawns "b5" [ "b6" ]

[<Fact>]
let ``push black pawn: b7-b1 (as if pawns slid)``() = 
    "8/1p6/8/8/8/8/8/8 b - - 0 1" |> findPushingPawns "b1" [ "b7" ]

[<Fact>]
let ``black pawns don't push back: b7-b8``() = 
    "8/1p6/8/8/8/8/8/8 b - - 0 1" |> findPushingPawns "b8" [ ]


[<Fact>]
let ``push white pawn: e2-e4``() = 
    "8/8/8/8/8/8/4P3/8 w - - 0 1" |> findPushingPawns "e4" [ "e2" ]

let findCapturingPawns square (expected : string list) board = 
    let _, scan, _ = sanScanners (ParseFen board |> unwrap)
    scan (_c square |> toX88)
    |> List.map CoordinateToString
    |> should equal expected

[<Fact>]
let ``white pawn captures: e2-d3``() = 
    "8/8/8/8/8/8/4P3/8 w - - 0 1" |> findCapturingPawns "d3" [ "e2" ]

[<Fact>]
let ``white pawn does not capture: e2-e3``() = 
    "8/8/8/8/8/8/4P3/8 w - - 0 1" |> findCapturingPawns "e3" [ ]

[<Fact>]
let ``white pawn does not capture backwards: e2-f1``() = 
    "8/8/8/8/8/8/4P3/8 w - - 0 1" |> findCapturingPawns "f1" [ ]

[<Fact>]
let ``2 black pawns can capture``() = 
    "8/8/8/8/8/8/2p1p3/8 b - - 0 1" |> findCapturingPawns "d1" [ "e2"; "c2" ]
