module GetLegalMoves

open CoordinateNotation
open Definitions
open MoveLegalityChecker

let FromSquare from position = 
    let rank7 = 1
    let rank2 = 6
    
    let p i = 
        PromotionMove({ Vector = i
                        PromoteTo = Queen })
    
    let u i = UsualMove(i)
    let f = from |> toX88
    let validate v t = position |> ValidateMove(v (from, t |> fromX88))
    let at88 i = position |> PieceAt (i |> fromX88)
    let gen v = 
        List.map (fun i -> f + i)
        >> List.filter (fun x -> (x &&& 0x88) = 0)
        >> List.map (fun t -> validate v t)
    
    let rec step start increment = 
      [ let curr = start + increment
        if curr &&& 0x88 = 0 then 
            yield validate u curr
            if at88 curr = None then 
                yield! step curr increment ]
    
    let iter li = li |> List.collect (step f)
    match position |> PieceAt from with
    | Some(White, Pawn) -> 
        if snd from = rank7 then gen p [ -16; -15; -17 ]
        else gen u [ -16; -32; -15; -17 ]
    | Some(Black, Pawn) -> 
        if snd from = rank2 then gen p [ +16; +15; +17 ]
        else gen u [ +16; +32; +15; +17 ]
    | Some(Black, Bishop) -> iter [ -15; +17; +15; -17 ]
    | _ -> []
    |> List.filter (fun m -> m.Hint.Errors |> List.isEmpty)
