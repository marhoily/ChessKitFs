module ChessKit.ChessLogic.FenPrinter

open Text
open System
open System.Text

let ToFen p = 
    let color = 
        match p.Core.ActiveColor with
        | White -> "w"
        | Black -> "b"
    
    let castling = 
        let result = 
            p.Core.CastlingAvailability
            |> List.map (function 
                   | WQ -> 'Q'
                   | WK -> 'K'
                   | BQ -> 'q'
                   | BK -> 'k')
            |> List.toArray
            // This can be just "System.String" in F# 4.0
            |> (fun arr -> (String(arr)))
        if result = "" then "-"
        else result
    
    let enPassant = 
        match p.Core.EnPassant with
        | Some(file) -> 
            (fileToStirng file) + (if p.Core.ActiveColor = Black then "3"
                                   else "6")
        | None -> "-"
    
    let sb = new StringBuilder()
    let appendc (c : char) = sb.Append(c) |> ignore
    let appends (s : string) = sb.Append(s) |> ignore
    let appendi (i : int) = sb.Append(i) |> ignore
    let mutable skip = 0
    let mutable file = 0
    for square in p.Core.Placement do
        if file = 8 then 
            appendc '/'
            file <- 0
        file <- file + 1
        match square with
        | Some p -> 
            if skip > 0 then 
                appendi skip
                skip <- 0
            appendc (pieceToChar p)
        | None -> skip <- skip + 1
        if file = 8 && skip > 0 then 
            appendi skip
            skip <- 0
    appendc ' '
    appends color
    appendc ' '
    appends castling
    appendc ' '
    appends enPassant
    appendc ' '
    appendi p.HalfMoveClock
    appendc ' '
    appendi p.FullMoveNumber
    string sb
