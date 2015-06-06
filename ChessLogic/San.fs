module San

open Definitions
open MoveLegalityChecker
open System.Text
open CoordinateNotation

// TODO: refactor Legal vs illegal moves?
let ToSanString (board : Position) (move : ValidatedMove) : string = 
    let typeToString = 
        function 
        | Pawn -> 'P'
        | Knight -> 'N'
        | Bishop -> 'B'
        | Rook -> 'R'
        | Queen -> 'Q'
        | King -> 'K'
    
    let decompose (m : ValidatedMove) = 
        match m.Move with
        | UsualMove(f, t) -> (f, t, Queen)
        | PromotionMove({ Vector = (f, t); PromoteTo = p }) -> (f, t, p)
    
    let piece = 
        match move.Hint.Piece with
        | Some(t) -> t
        | _ -> failwith "unexpected"
    
    let sb = new StringBuilder(6)
    let castling = move.Hint.Castling
    let (f, t, p) = decompose move
    let file x = fileToStirng (x |> fst)
    let rank x = rankToString (x |> snd)
    let fileAndRank = CoordinateToString
    let capture = move.Hint.Observations |> MyList.contains Capture
    let promotion = move.Hint.Observations |> MyList.contains Promotion
    let check = 
        move.Hint.ResultPosition.Value.Observations |> MyList.contains Check
    let mate = 
        move.Hint.ResultPosition.Value.Observations |> MyList.contains Mate
    let append (str : string) = sb.Append(str) |> ignore
    let appendc (str : char) = sb.Append(str) |> ignore
    
    let disambiguationList = 
        lazy ([ for move2 in board |> GetLegalMoves.All do
                    let (f2, t2, _) = decompose move2
                    let at x = board |> PieceAt x
                    if f <> f2 && t = t2 && at f = at f2 then yield f2 ])
    
    let ambiguous() = not (disambiguationList.Value |> List.isEmpty)
    let unique fn = 
        disambiguationList.Value 
        |> List.forall (fun f2 -> (f |> fn) <> (f2 |> fn))
    let shortCastling = castling = Some(WK) || castling = Some(BK)
    let longCastling = castling = Some(WQ) || castling = Some(BQ)
    // Actual algorithm
    if shortCastling then append "O-O"
    else if longCastling then append "O-O-O"
    else 
         if piece = Pawn then 
             if capture then append (file f)
         else 
             appendc (piece |> typeToString)
             if ambiguous() then 
                 if unique fst then append (file f)
                 else if unique snd then append (rank f)
                 else append (fileAndRank f)
         if capture then appendc 'x'
         append (fileAndRank t)
    if promotion then 
        appendc '='
        appendc (p |> typeToString)
    if check then appendc '+'
    else 
        if mate then appendc '#'
    string sb
