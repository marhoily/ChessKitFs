[<RequireQualifiedAccess>]
module ChessKit.ChessLogic.Board

let IsAttackedBy (side : Color) (position : PositionCore) coordinate = 
    ScanningExtensions.IsAttackedBy side position 
        (coordinate |> X88.fromCoordinate)
let FindKing (color : Color) (position : PositionCore) = 
    ScanningExtensions.FindKing color position
let IsInCheck (side : Color) (position : PositionCore) = 
    ScanningExtensions.IsInCheck side position
let ValidateMove (move : Move) (position : Position) = 
    MoveLegality.Validate move position
let ValidateLegalMove (move : Move) (position : Position) = 
    MoveLegality.ValidateLegal move position
let ParseLegalMove (move : string) (position : Position) = 
    MoveLegality.ParseLegal move position
