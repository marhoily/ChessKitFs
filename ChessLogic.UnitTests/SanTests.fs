﻿module SanTests

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
    |> ValidateMove(_cn move)
    |> ToSanString p
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
let ``Knight``() = 
    "r1b1kb1r/pp1n2pp/1q2p3/n2pPp2/2pP1P2/2P1BN2/PP1QB1PP/RN3RK1 w kq f6 0 11" 
    |> check "f3-g5" "Ng5"

    
    