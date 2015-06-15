module ChessKit.ChessLogic.GetLegalMoves

open CoordinateNotation
open Definitions
open MoveLegalityChecker

let FromSquare from position = 
    let legalOnly moves = 
        [ for move in moves do
              match move with
              | LegalMove legal -> yield legal
              | _ -> () ]
    
    let rank7 = 1
    let rank2 = 6
    
    let p (f,t) = Move.Create f t (Some(Queen))
    let u (f,t) = Move.Create f t None
    let f = from |> toX88
    let validate v t = position |> ValidateMove(v (from, t |> fromX88))
    let at88 i = position.Core |> PieceAt(i |> fromX88)
    
    let gen v = 
        List.map (fun i -> f + i)
        >> List.filter (fun x -> (x &&& 0x88) = 0)
        >> List.map (validate v)
    
    let rec step start increment = 
        [ let curr = start + increment
          if curr &&& 0x88 = 0 then 
              yield validate u curr
              if at88 curr = None then yield! step curr increment ]
    
    let iter = List.collect (step f)
    match position.Core |> PieceAt from with
    | Some(White, Pawn) -> 
        if snd from = rank7 then gen p [ -16; -15; -17 ]
        else gen u [ -16; -32; -15; -17 ]
    | Some(Black, Pawn) -> 
        if snd from = rank2 then gen p [ +16; +15; +17 ]
        else gen u [ +16; +32; +15; +17 ]
    | Some(_, Bishop) -> iter [ -15; +17; +15; -17 ]
    | Some(_, Rook) -> iter [ -1; +1; +16; -16 ]
    | Some(_, Queen) -> iter [ -15; +17; +15; -17; -1; +1; +16; -16 ]
    | Some(_, King) -> gen u [ -15; +17; +15; -17; -1; +1; +16; -16 ]
    | Some(_, Knight) -> gen u [ 33; 31; -33; -31; 18; 14; -18; -14 ]
    | None -> []
    |> legalOnly

let All (position : Position) = 
    [ for i = 0 to 63 do
          let square = (i % 8, i / 8)
          match position.Core |> PieceAt square with
          | Some(color, _) -> 
              if color = position.Core.ActiveColor then 
                  yield! position |> FromSquare square
          | _ -> () ]
