module Parsing

open FsUnit.Xunit
open ChessKit.ChessLogic

let ParseToStringShouldMatch toString parse input = 
    toString (Operators.getSuccess (parse input)) |> should equal input
let ErrorMessageShouldMatch parse (input : string) (msg : string) = 
    Operators.getError (parse input) |> should equal msg
