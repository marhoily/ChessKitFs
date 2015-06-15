module ChessKit.ChessLogic.FenPrinter

open Text
open System
open System.Text

let ToFen p = 
    let castlingHintToString = 
        function 
        | WQ -> 'Q'
        | WK -> 'K'
        | BQ -> 'q'
        | BK -> 'k'
    
    let printOutCastling c = 
        let result = 
            c
            |> List.map castlingHintToString
            |> List.toArray
            // This can be just "System.String" in F# 4.0
            |> (fun arr -> (String(arr)))
        if result = "" then "-"
        else result
    
    let printOutEnPassant c e = 
        match e with
        | Some(file) -> 
            (fileToStirng file) + (if c = Black then "3"
                                   else "6")
        | None -> "-"
    
    let sb = new StringBuilder()
    let mutable skip = 0
    let mutable file = 0
    for square in p.Core.Placement do
        if file = 8 then 
            sb.Append('/') |> ignore
            file <- 0
        file <- file + 1
        match square with
        | Some(p) -> 
            if skip > 0 then 
                sb.Append(skip) |> ignore
                skip <- 0
            sb.Append(PieceToString p) |> ignore
        | None -> skip <- skip + 1
        if file = 8 && skip > 0 then 
            sb.Append(skip) |> ignore
            skip <- 0
    sb.Append(' ') |> ignore
    sb.Append(p.Core.ActiveColor.AsString) |> ignore
    sb.Append(' ') |> ignore
    sb.Append(printOutCastling p.Core.CastlingAvailability) |> ignore
    sb.Append(' ') |> ignore
    sb.Append(printOutEnPassant p.Core.ActiveColor p.Core.EnPassant) |> ignore
    sb.Append(' ') |> ignore
    sb.Append(p.HalfMoveClock) |> ignore
    sb.Append(' ') |> ignore
    sb.Append(p.FullMoveNumber) |> ignore
    string sb
