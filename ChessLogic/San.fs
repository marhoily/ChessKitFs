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
    
    let sb = new StringBuilder(6)
    let castling = move.Hint.Castling
    
    let decompose m = 
        match m.Move with
        | UsualMove(f, t) -> (f, t, Queen)
        | PromotionMove({ Vector = (f, t); PromoteTo = p }) -> (f, t, p)
    
    let (f, t, p) = decompose move
    
    let piece = 
        match move.Hint.Piece with
        | Some(t) -> t
        | _ -> failwith "unexpected"
    
    let file x = 
        x
        |> fst
        |> printFile
    
    let rank x = 
        x
        |> snd
        |> rankToString
    
    let fileAndRank = CoordinateToString
    let capture = move.Hint.Observations |> MyList.contains Capture
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
                    else sb.Append(fileAndRank f) |> ignore
        if capture then sb.Append('x') |> ignore
        sb.Append(fileAndRank t) |> ignore
    if move.Hint.Observations |> MyList.contains Promotion then 
        sb.Append('=').Append(PieceToString(White, p)) |> ignore
    if move.Hint.ResultPosition.Value.Observations |> MyList.contains Check then 
        sb.Append('+') |> ignore
    else 
        if move.Hint.ResultPosition.Value.Observations |> MyList.contains Mate then 
            sb.Append('#') |> ignore
    string sb
