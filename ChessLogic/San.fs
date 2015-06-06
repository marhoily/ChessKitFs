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
    
    let sb = new StringBuilder(6)
    let shortCastling = move.Castling = Some(WK) || move.Castling = Some(BK)
    let longCastling = move.Castling = Some(WQ) || move.Castling = Some(BQ)
    let capture = move.Observations |> MyList.contains Capture
    let promotion = move.Observations |> MyList.contains Promotion
    let check = move.ResultPosition.Observations |> MyList.contains Check
    let mate = move.ResultPosition.Observations |> MyList.contains Mate
    let append (str : string) = sb.Append(str) |> ignore
    let appendc (str : char) = sb.Append(str) |> ignore
    let file x = fileToStirng (x |> fst)
    let rank x = rankToString (x |> snd)
    let fileAndRank = CoordinateToString
    let at x = board |> PieceAt x
    let isSimilarTo x y = 
        (x.Start <> y.Start) && (x.End = y.End) && (at x.Start = at y.Start)
    
    let disambiguationList = 
        lazy ([ for m in board |> GetLegalMoves.All do
                    if m |> isSimilarTo move then yield m.Start ])
    
    let ambiguous() = not (disambiguationList.Value |> List.isEmpty)
    let unique fn = 
        disambiguationList.Value 
        |> List.forall (fun x -> (move.Start |> fn) <> (x |> fn))
    // Actual algorithm
    if shortCastling then append "O-O"
    else if longCastling then append "O-O-O"
    else 
        if move.Piece = Pawn then 
            if capture then append (file move.Start)
        else 
            appendc (move.Piece |> typeToString)
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
