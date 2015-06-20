[<System.Runtime.CompilerServices.Extension>]
[<RequireQualifiedAccess>]
module ChessKit.ChessLogic.EndGame

open Scanning
open System.Runtime.CompilerServices
open Operators

let internal countMaterial (board : PositionCore) = 
    let white = Array.zeroCreate 5
    let black = Array.zeroCreate 5
    for i in 0..63 do
        let p = board.Placement.[i]
        if p <> None then 
            let piece = p.Value
            let square = Idx64.GetColor i
            
            let arr = 
                match piece |> fst with
                | Color.White -> white
                | Color.Black -> black
                | _ -> failwith "unexpected"
            
            let idx = 
                match piece |> snd, square with
                | PieceType.Pawn, _ | PieceType.Rook, _ | PieceType.Queen, _ -> 0
                | PieceType.Knight, _ -> 1
                | PieceType.Bishop, Color.White -> 2
                | PieceType.Bishop, Color.Black -> 3
                | PieceType.King, _ -> 4
                | _ -> failwith "unexpected"
            
            arr.[idx] <- arr.[idx] + 1
    white, black

[<Extension>]
let ToPosition(move : LegalMove) = 
    let core = move.ResultPosition
    let prev = move.OriginalPosition
    let piece = move.Piece
    let obs = move.Observations
    let color = prev.Core.ActiveColor
    
    let position = 
        { Core = core
          Move = Some(move)
          HalfMoveClock = 0
          FullMoveNumber = 0
          Properties = Properties.None }
    
    let newHalfMoveClock = 
        if piece = PieceType.Pawn || (obs |> test MoveObservations.Capture) then 0
        else prev.HalfMoveClock + 1
    
    let newMoveNumber = 
        prev.FullMoveNumber + if color = Color.Black then 1
                              else 0
    
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
        
        let a, b = core |> countMaterial
        isInsufficient a && isInsufficient b
    
    let newObs = 
        [ if isCheck && noMoves then yield Properties.Mate
          if isCheck && not noMoves then yield Properties.Check
          if not isCheck && noMoves then yield Properties.Stalemate
          if isRepetition then yield Properties.Repetition
          if insufficientMaterial then yield Properties.InsufficientMaterial
          if prev.HalfMoveClock >= 50 then yield Properties.FiftyMoveRule ]
        |> List.fold (|||) Properties.None
    
    { Core = core
      HalfMoveClock = newHalfMoveClock
      FullMoveNumber = newMoveNumber
      Move = Some(move)
      Properties = newObs }
