namespace ChessKit.ChessLogic

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
