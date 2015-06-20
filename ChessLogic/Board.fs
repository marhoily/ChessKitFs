[<System.Runtime.CompilerServices.Extension>]
[<RequireQualifiedAccess>]
module ChessKit.ChessLogic.BoardExtensions

open System.Runtime.CompilerServices

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

[<Extension>]
let ValidateLegalMove (position : Position) (move : Move) = 
    MoveLegality.ValidateLegal move position

[<Extension>]
let ParseLegalMove (position : Position) (move : string) = 
    MoveLegality.ParseLegal move position
