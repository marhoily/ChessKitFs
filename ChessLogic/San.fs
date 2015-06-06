module San

open Definitions
open MoveLegalityChecker
open System.Text
open CoordinateNotation

// TODO: refactor Legal vs illegal moves?
let ToSanString (board : Position) (move : LegalMove) : string = 
    let typeToString = 
        function 
        | Pawn -> 'P'
        | Knight -> 'N'
        | Bishop -> 'B'
        | Rook -> 'R'
        | Queen -> 'Q'
        | King -> 'K'
    
    let decompose m = (m.Start, m.End, m.PromoteTo)
    let piece = move.Piece
    let sb = new StringBuilder(6)
    let castling = move.Castling
    //let (f, t, p) = (move.Start, move.End, move.PromoteTo)
    let file x = fileToStirng (x |> fst)
    let rank x = rankToString (x |> snd)
    let fileAndRank = CoordinateToString
    let capture = move.Observations |> MyList.contains Capture
    let promotion = move.Observations |> MyList.contains Promotion
    let check = move.ResultPosition.Observations |> MyList.contains Check
    let mate = move.ResultPosition.Observations |> MyList.contains Mate
    let append (str : string) = sb.Append(str) |> ignore
    let appendc (str : char) = sb.Append(str) |> ignore
    
    let disambiguationList = 
        lazy ([ for move2 in board |> GetLegalMoves.All do
                    let (f2, t2, _) = decompose move2
                    let at x = board |> PieceAt x
                    if move.Start <> f2 && move.End = t2 
                       && at move.Start = at f2 then yield f2 ])
    
    let ambiguous() = not (disambiguationList.Value |> List.isEmpty)
    let unique fn = 
        disambiguationList.Value 
        |> List.forall (fun f2 -> (move.Start |> fn) <> (f2 |> fn))
    let shortCastling = castling = Some(WK) || castling = Some(BK)
    let longCastling = castling = Some(WQ) || castling = Some(BQ)
    // Actual algorithm
    if shortCastling then append "O-O"
    else if longCastling then append "O-O-O"
    else 
        if piece = Pawn then 
            if capture then append (file move.Start)
        else 
            appendc (piece |> typeToString)
            if ambiguous() then 
                if unique fst then append (file move.Start)
                else if unique snd then append (rank move.Start)
                else append (fileAndRank move.Start)
        if capture then appendc 'x'
        append (fileAndRank move.End)
    if promotion then 
        appendc '='
        appendc (move.PromoteTo |> typeToString)
    if check then appendc '+'
    else 
        if mate then appendc '#'
    string sb
