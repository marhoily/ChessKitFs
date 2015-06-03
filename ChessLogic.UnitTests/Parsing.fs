module Parsing

open FsUnit.Xunit
open Parsing
open System.Text

let shouldEqual (a:string) (b:string) = 
    if a <> b then
        printfn "expected:"
        printfn "%A" (a.Replace(" ", "·"))
        printfn "%A" (Encoding.ASCII.GetBytes(a))
        printfn "but was:"
        printfn "%A" (Encoding.ASCII.GetBytes(b))
        failwith "should equal failed"
let ParseToStringShouldMatch toString parse input = 
    toString (unwrap (parse input)) |> should equal input
let ErrorMessageShouldMatch parse (input : string) (msg : string) = 
    getErrorMessage (parse input) |> shouldEqual msg
