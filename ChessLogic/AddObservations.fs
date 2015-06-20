module ChessKit.ChessLogic.AddObservations

open ScanningExtensions

let GetSquareColor(c : int) = 
    let file, rank = c % 8, c / 8
    if (file % 2) = (rank % 2) then White
    else Black

let internal CountMaterial(board : PositionCore) = 
    let white = Array.zeroCreate 5
    let black = Array.zeroCreate 5
    for i in 0..63 do
        let p = board.Placement.[i]
        if p <> None then 
            let piece = p.Value
            let square = GetSquareColor i
            
            let arr = 
                match piece |> fst with
                | White -> white
                | Black -> black
            
            let idx = 
                match piece |> snd, square with
                | Pawn, _ | Rook, _ | Queen, _ -> 0
                | Knight, _ -> 1
                | Bishop, White -> 2
                | Bishop, Black -> 3
                | King, _ -> 4
            
            arr.[idx] <- arr.[idx] + 1
    white, black

let internal positionFromCoreAndMove core move = 
    { Core = core
      Move = Some(move)
      HalfMoveClock = 0
      FullMoveNumber = 0
      Observations = [] }

let CoreToPosition(move : LegalMove) = 
    let core = move.ResultPosition
    let prev = move.OriginalPosition
    let piece = move.Piece
    let obs = move.Observations
    let color = prev.Core.ActiveColor
    let position = positionFromCoreAndMove core move
    
    let newHalfMoveClock = 
        if piece = Pawn || obs |> List.contains Capture then 0
        else prev.HalfMoveClock + 1
    
    let newMoveNumber = 
        prev.FullMoveNumber + if color = Black then 1
                              else 0
    
    let newAt x = core.Placement.[x |> Coordinate.toIdx64]
    let isCheck = IsInCheck core.ActiveColor core
    let noMoves = (position |> GetLegalMoves.All).IsEmpty
    
    let isRepetition = 
        let rec toSequence pos = 
            seq { 
                yield pos.Core
                if pos.Move <> None then 
                    let next = pos.Move.Value.OriginalPosition
                    yield! toSequence next
            }
        toSequence position
        |> Seq.countBy id
        |> Seq.map snd
        |> Seq.max
        > 2
    
    let insufficientMaterial = 
        let isInsufficient material = 
            // Other, Knight, White Bishop, Black Bishop, King
            match material with
            | [| 0; 0; 0; _; 1 |] -> true // any number of black bishops
            | [| 0; 0; _; 0; 1 |] -> true // any number of white bishops
            | [| 0; 1; 0; 0; 1 |] -> true // one knight
            | _ -> false
        
        let a, b = core |> CountMaterial
        isInsufficient a && isInsufficient b
    
    let newObs = 
        [ if isCheck && noMoves then yield Mate
          if isCheck && not noMoves then yield Check
          if not isCheck && noMoves then yield Stalemate
          if isRepetition then yield Repetition
          if insufficientMaterial then yield InsufficientMaterial
          if prev.HalfMoveClock >= 50 then yield FiftyMoveRule ]
    
    { Core = core
      HalfMoveClock = newHalfMoveClock
      FullMoveNumber = newMoveNumber
      Move = Some(move)
      Observations = newObs }
