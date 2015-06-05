﻿module GetLegalMoves

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
    
    let gen v = 
        List.map (fun i -> (from |> toX88) + i)
        >> List.filter (fun x -> (x &&& 0x88) = 0)
        >> List.map (fun t -> position |> ValidateMove(v (from, t |> fromX88)))
    match position |> PieceAt from with
    | Some(White, Pawn) -> 
        if snd from = rank7 then gen p [ -16; -15; -17 ]
        else gen u [ -16; -32; -15; -17 ]
    | Some(Black, Pawn) -> 
        if snd from = rank2 then gen p [ +16; +15; +17 ]
        else gen u [ +16; +32; +15; +17 ]
    | _ -> []
    |> List.filter (fun m -> m.Hint.Errors |> List.isEmpty)
