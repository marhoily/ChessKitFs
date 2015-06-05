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
    
    let decompose m = 
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
    let file x = printFile (x |> fst)
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
    if castling = Some(WK) || castling = Some(BK) then 
        sb.Append("O-O") |> ignore
    else if castling = Some(WQ) || castling = Some(BQ) then 
        sb.Append("O-O-O") |> ignore
    else 
        if piece = Pawn then 
            if capture then sb.Append(file f) |> ignore
        else 
            sb.Append(piece |> typeToString) |> ignore
            let disambiguationList = 
                [ for move2 in board |> GetLegalMoves.All do
                      let (f2, t2, _) = decompose move2
                      let at x = board |> PieceAt x
                      if f <> f2 && t = t2 && at f = at f2 then yield f2 ]
            if not (disambiguationList |> List.isEmpty) then 
                let uniqueFile = 
                    disambiguationList 
                    |> List.forall (fun f2 -> (f |> fst) <> (f2 |> fst))
                if uniqueFile then sb.Append(file f) |> ignore
                else 
                    let uniqueRank = 
                        disambiguationList 
                        |> List.forall (fun f2 -> (f |> snd) <> (f2 |> snd))
                    if uniqueRank then sb.Append(rank f) |> ignore
                    else append(fileAndRank f)
        if capture then appendc 'x'
        append(fileAndRank t)
    if promotion then appendc '='; appendc(PieceToString(White, p)) 
    if check then appendc '+'
    else if mate then appendc '#'
    string sb
