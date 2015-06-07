module San

open Definitions
open MoveLegalityChecker
open Parsing
open System.Text
open CoordinateNotation
open FParsec

let ToSanString(move : LegalMove) = 
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
    let file, rank, fileAndRankStr = fst, snd, CoordinateToString
    let fileStr x = fileToStirng (x |> file)
    let rankStr x = rankToString (x |> rank)
    let at x = move.OriginalPosition |> PieceAt x
    let isSimilarTo x y = 
        (x.Start <> y.Start) && (x.End = y.End) && (at x.Start = at y.Start)
    
    let disambiguationList = 
        lazy ([ for m in move.OriginalPosition |> GetLegalMoves.All do
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
            if capture then append (fileStr move.Start)
        else 
            appendc (move.Piece |> typeToString)
            if ambiguous() then 
                if unique file then append (fileStr move.Start)
                else if unique rank then append (rankStr move.Start)
                else append (fileAndRankStr move.Start)
        if capture then appendc 'x'
        append (fileAndRankStr move.End)
    if promotion then 
        appendc '='
        appendc (move.PromoteTo |> typeToString)
    if check then appendc '+'
    else 
        if mate then appendc '#'
    string sb

type Ending = 
    | Check
    | Mate

type Capture = 
    | Capture

type Hint = 
    | FileHint of File
    | RankHint of Rank
    | SquareHint of Coordinate
    | NoHint

type Moves = 
    | ShortCastling
    | LongCastling
    | PawnPush of Coordinate * PieceType option
    | PawnCapture of File * (Coordinate * PieceType option)
    | Usual of (PieceType * (Hint * (Capture option * Coordinate)))

let ParseSanString str = 
    let parseFile = LetterToFileNoCheck
    let parseRank (c : char) : Rank = (int '8') - (int c)
    
    let parsePiece = 
        function 
        | 'N' -> Knight
        | 'B' -> Bishop
        | 'R' -> Rook
        | 'Q' -> Queen
        | 'K' -> Queen
        | _ -> failwith ("unknown promotion hint")
    
    let short = stringReturn "O-O" ShortCastling
    let long = stringReturn "O-O-O" LongCastling
    let check = charReturn '+' Check
    let mate = charReturn '#' Mate
    let file = anyOf "abcdefgh" |>> parseFile
    let rank = anyOf "12345678" |>> parseRank
    let piece = anyOf "NBRQK" |>> parsePiece
    let capture = anyOf "x:" >>% Capture
    let promotion = skipChar '=' >>. anyOf "NBRQ" |>> parsePiece
    let ending = check <|> mate
    let square = file .>>. rank
    let pawn = square .>>. opt promotion
    let pawnPush = pawn |>> PawnPush
    let pawnCapture = attempt (file .>> capture .>>. pawn) |>> PawnCapture
    let target = opt capture .>>. square
    let squareHint = attempt square |>> SquareHint
    let fileHint = file |>> FileHint
    let rankHint = rank |>> RankHint
    let hint = choice [ squareHint; fileHint; rankHint ]
    let hinted = attempt (hint .>>. target)
    let hintless = preturn NoHint .>>. target
    let move = piece .>>. (hinted <|> hintless) |>> Usual
    let moves = choice [ long; short; move; pawnCapture; pawnPush ]
    let san = moves .>>. opt ending
    run san str

type SanError = 
    | PieceNotFound of PieceType
    | DisambiguationIsMissing of Coordinate list

type SanWarning = 
    | IsCapture
    | IsNotCapture
    | IsCheck
    | IsNotCheck
    | IsMate
    | IsNotMate
    | PromotionHintIsMissing
    | PromotionHintIsExcessive
    | DisambiguationIsExcessive

type SanMove = 
    | Interpreted of MoveInfo * SanWarning list
    | Nonsense of SanError
    | Unparsable of string

let FromSanString str board = 
    let castling opt = 
        let proto = 
            match board.ActiveColor, opt with
            | White, ShortCastling -> "e1-g1"
            | White, LongCastling -> "e1-c1"
            | Black, ShortCastling -> "e8-g8"
            | Black, LongCastling -> "e8-c8"
            | _ -> failwith "unexpected"
        Interpreted(board |> ValidateMove(_cn (proto)), [])
    
    let convert = 
        function 
        | ShortCastling, notes -> castling ShortCastling
        | LongCastling, notes -> castling LongCastling
        | PawnPush(toSquare, promoteTo), notes -> Unparsable("blah")
        | PawnCapture(fromFile, (toSquare, promoteTo)), notes -> 
            Unparsable("blah")
        | Usual(piece, (hint, (capture, toSquare))), notes -> Unparsable("blah")
    
    match ParseSanString str with
    | Success(p, _, _) -> Result.Ok(convert p)
    | Failure(e, _, _) -> Result.Error(e)
