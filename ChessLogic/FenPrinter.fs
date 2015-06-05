module FenPrinter

open Definitions
open System.Text

let ToFen p = 
    let printOutCastling c = 
        let result = 
            c
            |> List.map CastlingHint.toString
            |> List.toArray
            // This can be just "System.String" in F# 4.0
            |> (fun arr -> (new System.String(arr))) 
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
    for square in p.Placement do
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
    sb.Append(p.ActiveColor |> Color.toString) |> ignore
    sb.Append(' ') |> ignore
    sb.Append(printOutCastling p.CastlingAvailability) |> ignore
    sb.Append(' ') |> ignore
    sb.Append(printOutEnPassant p.ActiveColor p.EnPassant) |> ignore
    sb.Append(' ') |> ignore
    sb.Append(p.HalfMoveClock) |> ignore
    sb.Append(' ') |> ignore
    sb.Append(p.FullMoveNumber) |> ignore
    string sb
