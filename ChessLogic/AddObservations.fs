module AddObservations

open Definitions
open MyList
open CoordinateNotation
open IsAttackedBy
open MoveLegalityChecker

let CoreToPosition(move : MoveSrc<LegalMove>) = 
    let core = move.Data.ResultPosition
    let prev = move.OriginalPosition
    let piece = move.Data.Piece
    let obs = move.Data.Observations
    let color = prev.Core.ActiveColor
    
    let newHalfMoveClock = 
        if piece = Pawn || obs |> contains Capture then 0
        else prev.HalfMoveClock + 1
    
    let newMoveNumber = 
        prev.FullMoveNumber + if color = Black then 1
                              else 0
    
    let newAt x = core.Placement.[x |> ToIndex]
    let isCheck = IsInCheck core.ActiveColor newAt
    
    let isNotMate() = 
        seq { 
            for i = 0 to 7 do
                for j = 0 to 7 do
                    for k = 0 to 7 do
                        for l = 0 to 7 do
                            let move = Move.Create (i, j) (k, l) None
                            let res = core |> ValidateMoveRaw move
                            match res with
                            | LegalMove _ -> yield true
                            | IllegalMove _ -> yield false
        }
        |> Seq.exists id
    
    let newObs = 
        if isCheck then 
            if isNotMate() then [ Check ]
            else [ Check; Mate ]
        else []
    
    { Core = core
      HalfMoveClock = newHalfMoveClock
      FullMoveNumber = newMoveNumber
      Move = Some(move)
      Observations = newObs }
