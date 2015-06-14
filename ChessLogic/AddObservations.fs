module AddObservations

open Definitions
open MyList
open CoordinateNotation
open IsAttackedBy

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
    
    let isMate() = 
        Position.FromCore core
        |> GetLegalMoves.All
        |> List.isEmpty
    
    let isRepetition() = 
        let rec toSequence pos = 
            seq { 
                yield pos.Core
                if pos.Move <> None then 
                    let next = pos.Move.Value.OriginalPosition
                    yield! toSequence next
            }
        toSequence (Position.FromCoreAndMove core move)
        |> Seq.countBy id
        |> Seq.map snd
        |> Seq.max
        > 2
    
    let checkOrMate = 
        if isCheck then 
            if isMate() then [ Mate ]
            else [ Check ]
        else []
    
    let newObs = 
        if isRepetition() then Repetition :: checkOrMate
        else checkOrMate
    
    { Core = core
      HalfMoveClock = newHalfMoveClock
      FullMoveNumber = newMoveNumber
      Move = Some(move)
      Observations = newObs }
