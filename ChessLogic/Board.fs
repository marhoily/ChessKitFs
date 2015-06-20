module ChessKit.ChessLogic.Board

let IsAttackedBy (side:Color) (position: PositionCore) coordinate = 
    ScanningExtensions.IsAttackedBy side position (coordinate |> X88.fromCoordinate)    
let FindKing (color:Color) (position: PositionCore) = 
    ScanningExtensions.FindKing color position
let IsInCheck (side:Color) (position: PositionCore) = 
    ScanningExtensions.IsInCheck side position
