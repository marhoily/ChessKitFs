module Parsing

open FsUnit.Xunit
open Parsing

let ParseToStringShouldMatch toString parse input = 
    toString (unwrap (parse input)) |> should equal input
let ErrorMessageShouldMatch parse (input : string) (msg : string) = 
    getErrorMessage (parse input) |> should equal msg
