module Parsing

open FsUnit.Xunit
open Parsing

let shouldEqual (a:string) (b:string) = 
    if a <> b then
        printfn "expected:"
        printfn "%A" (a.Replace(" ", "·"))
        printfn "but was:"
        printfn "%A" (b.Replace(" ", "·"))
        failwith "should equal failed"
let ParseToStringShouldMatch toString parse input = 
    toString (unwrap (parse input)) |> should equal input
let ErrorMessageShouldMatch parse (input : string) (msg : string) = 
    getErrorMessage (parse input) |> shouldEqual msg
