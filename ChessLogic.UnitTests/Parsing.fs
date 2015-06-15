module Parsing

open FsUnit.Xunit
open ChessKit.ChessLogic

let shouldEqual (actual:string) (expected:string) = 
    let a = actual.Replace("\r\n", "\n")
    let b = expected.Replace("\r\n", "\n")
    if a <> b then
        printfn "expected:"
        printfn "%A" a//(a.Replace(" ", "·"))
        printfn "but was:"
        printfn "%A" b//(Encoding.ASCII.GetBytes(b))
//        if a.Length <> b.Length then
//            printfn "letngths do not match. expected: %A, but was %A" a.Length b.Length
//        else
//            let mutable position = 0
//            let cmp x y =
//                if x <> y then
//                    printfn "symbols at index %A do not match: '%A' '%A'" position x y
//                position <- position + 1
//            Seq.iter2 cmp a b
//                

        failwith "should equal failed"
let ParseToStringShouldMatch toString parse input = 
    toString (Operators.getSuccess (parse input)) |> should equal input
let ErrorMessageShouldMatch parse (input : string) (msg : string) = 
    Operators.getError (parse input) |> should equal msg
