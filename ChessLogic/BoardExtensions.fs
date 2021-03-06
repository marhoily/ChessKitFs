﻿namespace ChessKit.ChessLogic

open System.Runtime.CompilerServices

[<Extension>]
[<RequireQualifiedAccess>]
module BoardExtensions = 
    [<Extension>]
    let IsAttackedBy (position : PositionCore) (side : Color) idx64 = 
        Scanning.IsAttackedBy side position (idx64 |> X88.fromIdx64)
    
    [<Extension>]
    let FindKing (position : PositionCore) (color : Color) = 
        Scanning.FindKing color position
    
    [<Extension>]
    let IsInCheck (position : PositionCore) (side : Color) = 
        Scanning.IsInCheck side position
    
    [<Extension>]
    let ValidateMove (position : Position) (move : Move) = 
        MoveLegality.Validate move position
    
    /// Parses move, throws when illegal
    [<Extension>]
    let ValidateLegalMove (position : Position) (move : Move) = 
        MoveLegality.ValidateLegal move position
    
    /// Parses string like "e2-e4", throws when illegal
    [<Extension>]
    let ParseLegalMove (position : Position) (move : string) = 
        MoveLegality.ParseLegal move position
    
    /// Parses string like "e4", throws when illegal
    [<Extension>]
    let ParseSanMove (position : Position) (move : string) = 
        San.Parse move position

    /// Parses string like "e4", throws when illegal
    [<Extension>]
    let MakeMove (position : Position) (move : string) = 
        San.Parse move position |> EndGame.ToPosition

[<RequireQualifiedAccess>]
module Board = 
    let StartingPosition = 
        Fen.Parse "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
