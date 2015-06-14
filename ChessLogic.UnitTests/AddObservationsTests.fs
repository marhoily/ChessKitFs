module AddObservationsTests

open AddObservations
open FenParser
open Parsing
open MoveLegalityChecker
open FsUnit.Xunit
open CoordinateNotation
open Microsoft.FSharp.Reflection
open Definitions
open Xunit

let observationsToString (pos : Position) = 
    let toString (x : 'a) = 
        match FSharpValue.GetUnionFields(x, typeof<'a>) with
        | case, _ -> case.Name
    
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

[<Fact>]
let ``Gives check``() = 
    checkObservations "8/2Rk4/1q4BP/8/8/6K1/8/8 b - - 24 119" "b6-c7" "Check"

[<Fact>]
let ``Gives mate``() = 
    checkObservations "2K5/8/2k4r/8/8/8/8/8 b - - 0 9" "h6-h8" "Check | Mate"
