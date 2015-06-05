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
    |> ValidateMove(_cn move)
    |> ToSanString p
    |> should equal expectedSan

[<Fact>]
let ``O-O``() = 
    "r3kb1r/pppb1pp1/8/7p/3PPBn1/2N5/PPP3PP/R2QK2R w KQkq - 0 14" 
    |> check "e1-g1" "O-O"
