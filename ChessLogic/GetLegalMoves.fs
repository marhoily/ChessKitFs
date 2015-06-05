module GetLegalMoves

open CoordinateNotation
open Definitions
open MoveLegalityChecker

let FromSquare from position =
    let m i = (from, (from |> toX88) + i |> fromX88)
    let p i = PromotionMove({ Vector = m i
                              PromoteTo = Queen})
    let u i = UsualMove(m i)
    let g v i = position |> ValidateMove (v -16)
    match position |> PieceAt from  with
    | Some(White, Pawn) -> 
        if snd from = 1 then
            [g p -16]
        else
            [g u -16]
    | _ -> []
    |> List.filter (fun m -> m.Hint.Errors |> List.length = 0)