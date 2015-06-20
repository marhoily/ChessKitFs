namespace ChessKit.ChessLogic

module internal List = 
    open System.Collections.Generic
    
    // Can replace with List.contains in F# 4.0
    let contains item = List.exists (fun i -> i = item)
    
    // Can replace with List.except in F# 4.0
    let except (itemsToExclude : 'T list) list = 
        let cached = HashSet<'T>(itemsToExclude, HashIdentity.Structural)
        list |> List.filter cached.Add

module internal Operators = 
    open FParsec
    
    let getSuccess = 
        function 
        | Success(x, _, _) -> x
        | Failure(x, _, _) -> failwith x

    let getError = 
        function 
        | Success(x, _, _) -> failwithf "expected error, but get: %A" x
        | Failure(x, _, _) -> x
    
    let inline (?|?) a b = 
        match a with
        | Some(x) -> x
        | None -> b

    let inline test (a : 'U when 'U : enum<int32>) (b : 'U when 'U : enum<int32>) =
        a &&& b <> enum<'U>(0)
