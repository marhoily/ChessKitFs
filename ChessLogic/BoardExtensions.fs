namespace ChessKit.ChessLogic

open System.Runtime.CompilerServices

[<Extension>]
[<RequireQualifiedAccess>]
module BoardExtensions = 
    [<Extension>]
    let IsAttackedBy (position : PositionCore) (side : Color) coordinate = 
        ScanningExtensions.IsAttackedBy side position 
            (coordinate |> X88.fromCoordinate)
    
    [<Extension>]
    let FindKing (position : PositionCore) (color : Color) = 
        ScanningExtensions.FindKing color position
    
    [<Extension>]
    let IsInCheck (position : PositionCore) (side : Color) = 
        ScanningExtensions.IsInCheck side position
    
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
