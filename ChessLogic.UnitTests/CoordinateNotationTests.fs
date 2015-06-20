module CoordinateNotationTests

open Xunit
open FsUnit.Xunit
open Parsing
open ChessKit.ChessLogic
open ChessKit.ChessLogic.CoordinateNotation

let positive = 
    ParseToStringShouldMatch Text.moveToString ParseCoordinateNotation
let negative = ErrorMessageShouldMatch TryParseCoordinateNotation

[<Fact>]
let ``(4,6) -> (4,4) should read "e2-e4"``() = 
    Move.Create (4, 6) (4, 4) None
    |> Text.moveToString
    |> should equal "e2-e4"

[<Fact>]
let ``Promotion move should read correctly``() = 
    Move.Create (4, 6) (4, 4) (Some(Queen))
    |> Text.moveToString
    |> should equal "e2-e4=Q"

[<Fact>]
let ``works with e2-e4``() = positive "e2-e4"

[<Fact>]
let ``works with a1-h8``() = positive "a1-h8"

[<Fact>]
let ``works with e2-e4=Q``() = positive "e2-e4=Q"

[<Fact>]
let ``works with a1-h8=Q``() = positive "a1-h8=Q"

[<Fact>]
let ``meaningful error for a1h8``() = negative "a1h8" "Error in Ln: 1 Col: 3
a1h8
  ^
Expecting: '-' or 'x'
"
