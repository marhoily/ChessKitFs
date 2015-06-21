[<RequireQualifiedAccess>]
module ChessKit.ChessLogic.GetLegalMoves

open PositionCoreExt

let FromSquare (fromIdx64 : int) position = 
    let legalOnly moves = 
        [ for move in moves do
              match move with
              | LegalMove legal -> yield legal
              | _ -> () ]
    
    let rank7 = 1
    let rank2 = 6
    let p (f, t) = Move.Create f t PieceType.Queen
    let u (f, t) = Move.Create f t PieceType.None
    let fromX88 = fromIdx64 |> X88.fromIdx64
    
    let validate createMove toX88 = 
        let toCoordinate = toX88 |> Idx64.fromX88
        let move = createMove (fromIdx64, toCoordinate)
        position |> MoveLegality.Validate(move)
    
    let atX88 = position.Core.atX88
    
    let gen createMove = 
        List.map ((+) fromX88)
        >> List.filter (fun x -> (x &&& 0x88) = 0)
        >> List.map (validate createMove)
    
    let rec step start increment = 
        [ let curr = start + increment
          if curr &&& 0x88 = 0 then 
              yield validate u curr
              if atX88 curr = Piece.EmptyCell then yield! step curr increment ]
    
    let iter = List.collect (step fromX88)
    let piece = position.Core.atIdx64 fromIdx64
    match piece |> getColor, piece |> getPieceType with
    | Color.White, PieceType.Pawn -> 
        if Idx64.Rank fromIdx64 = rank7 then gen p [ -16; -15; -17 ]
        else gen u [ -16; -32; -15; -17 ]
    | Color.Black, PieceType.Pawn -> 
        if Idx64.Rank fromIdx64 = rank2 then gen p [ +16; +15; +17 ]
        else gen u [ +16; +32; +15; +17 ]
    | _, PieceType.Bishop -> iter [ -15; +17; +15; -17 ]
    | _, PieceType.Rook -> iter [ -1; +1; +16; -16 ]
    | _, PieceType.Queen -> iter [ -15; +17; +15; -17; -1; +1; +16; -16 ]
    | _, PieceType.King -> gen u [ -15; +17; +15; -17; -1; +1; +16; -16 ]
    | _, PieceType.Knight -> gen u [ 33; 31; -33; -31; 18; 14; -18; -14 ]
    | _, PieceType.None -> []
    | _ -> failwith "unexpected"
    |> legalOnly

let All(position : Position) = 
    [ for idx64 = 0 to 63 do
          let colr = position.Core.atIdx64 idx64 |> getColor
          if colr = position.Core.ActiveColor then 
              yield! position |> FromSquare idx64 ]
