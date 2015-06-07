module SanTests

open FsUnit.Xunit
open Xunit
open Parsing
open MoveLegalityChecker
open FenParser
open CoordinateNotation
open San

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
let ``Knight``() = 
    "r1b1kb1r/pp1n2pp/1q2p3/n2pPp2/2pP1P2/2P1BN2/PP1QB1PP/RN3RK1 w kq f6 0 11" 
    |> check "f3-g5" "Ng5"

[<Fact>]
let ``Check``() = 
    "2kr4/pp2n1p1/6qr/n3P3/1NpP1p2/2P2B2/PQ3B1P/Rb3K2 b - - 0 28" 
    |> check "b1-d3" "Bd3+"

[<Fact>]
let ``Promotion``() = 
    "5r2/ppkP2p1/4P3/n2n1q2/1Np2p2/2P2Q2/P4B1r/R3K3 w - - 0 36" 
    |> check "d7-d8=N" "d8=N"

[<Fact>]
let ``Nec6``() = 
    "2kr4/pp2n1p1/6qr/n3P3/1NpP1p2/2P2B2/PQ3B1P/Rb3K2 b - - 0 28" 
    |> check "e7-c6" "Nec6"

[<Fact>]
let ``N7c6``() = 
    "2kr4/pp2n1p1/6qr/4n3/2pP1p2/2PN1B2/PQ3B1P/R4K2 b - - 0 29" 
    |> check "e7-c6" "N7c6"
    
[<Fact>]
let Ne5c6() = 
    "2kr4/pp2n1p1/6qr/n3n3/2pP1p2/2PN1B2/PQ3B1P/R4K2 b - - 0 29" 
    |> check "e5-c6" "Ne5c6"
    


    