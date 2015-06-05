module GetLegalMoves

open CoordinateNotation
open Definitions
open MoveLegalityChecker

let FromSquare from position =
    let fP = position |> PieceAt from 
    let fromSquare = toX88 from
    let at p c = p |> PieceAt (fromX88 c)
    match fP with
    | Some(White, Pawn) -> 
        if snd from = 1 then
            [position |> ValidateMove (
                PromotionMove({ 
                    Vector = (from, (from |> toX88) - 16 |> fromX88);
                    PromoteTo = Queen}))]
        else
            [position |> ValidateMove (UsualMove(from, (from |> toX88) - 16 |> fromX88))]
    | _ -> []
    |> List.filter (fun m -> m.Hint.Errors |> List.length = 0)