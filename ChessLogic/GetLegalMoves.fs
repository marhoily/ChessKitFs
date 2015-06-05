module GetLegalMoves

open CoordinateNotation
open Definitions
open MoveLegalityChecker

let FromSquare from position =
    let rank7 = 1
    let m i = (from, (from |> toX88) + i |> fromX88)
    let p i = PromotionMove({ Vector = m i; PromoteTo = Queen})
    let u i = UsualMove(m i)
    let g v i = position |> ValidateMove (v i)
    let gen v il = il |> List.map (g v)
    match position |> PieceAt from  with
    | Some(White, Pawn) -> 
        if snd from = rank7 then
            gen p [-16; -15; -17]
        else
            gen u [-16; -32; -15; -17]
    | _ -> []
    |> List.filter (fun m -> m.Hint.Errors |> List.isEmpty)