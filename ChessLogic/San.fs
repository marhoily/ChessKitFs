module San

open Definitions
open MoveLegalityChecker
open Parsing
open System.Text
open CoordinateNotation
open FParsec
open IsAttackedBy

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
    | SanCheck
    | SanMate

type SanCapture = 
    | SanCapture

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
    | Usual of (PieceType * (Hint * (SanCapture option * Coordinate)))

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
    let check = charReturn '+' SanCheck
    let mate = charReturn '#' SanMate
    let file = anyOf "abcdefgh" |>> parseFile
    let rank = anyOf "12345678" |>> parseRank
    let piece = anyOf "NBRQK" |>> parsePiece
    let capture = anyOf "x:" >>% SanCapture
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
    | PieceNotFound of Piece
    | AmbiguousChoice of Coordinate list

type SanWarning = 
    | IsCapture
    | IsNotCapture
    | IsCheck
    | IsNotCheck
    | IsMate
    | IsNotMate
    | DisambiguationIsExcessive

type SanMove = 
    | Interpreted of MoveInfo * SanWarning list
    | Nonsense of SanError
    | Unparsable of string

let sanScanners board = 
    let at88 i = board |> PieceAt(i |> fromX88)
    let color = board.ActiveColor
    let project = 
        Seq.map (fun f -> f())
        >> Seq.filter (fun x -> x <> -1)
        >> Seq.map fromX88
        >> Seq.toList

    let findPushingPawns square = 
        let _, slide = getScanners color at88 square
        match color with
        | Black -> slide Pawn [ -16; ]
        | White -> slide Pawn [ +16; ]
        |> project

    let findCapturingPawns square = 
        let one, _ = getScanners color at88 square
        match color with
        | Black -> one Pawn [ -15; -17 ]
        | White -> one Pawn [ +15; +17 ]
        |> project

    let findNonPawnPieces ofType square = 
        let one, scan = getScanners color at88 square
        match ofType with
        | Knight -> one Knight [ -33; -31; -18; -14; +33; +31; +18; +14 ]
        | Queen -> scan Queen [ +15; +17; -15; -17; +16; +01; -16; -01 ]
        | Rook -> scan Rook [ +16; +01; -16; -01 ]
        | Bishop -> scan Bishop [ +15; +17; -15; -17 ]
        | King -> one King [ +15; +17; -15; -17; +16; +01; -16; -01 ]
        | Pawn -> failwith "unexpected"
        |> project
    (findPushingPawns, findCapturingPawns, findNonPawnPieces)

let FromSanString str board = 
    let color = board.ActiveColor
    let findPushingPawns, findCapturingPawns, findNonPawnPieces = 
        sanScanners board
    
    let disambiguate hint candidates = 
        let disambiguator = 
            match hint with
            | FileHint f -> (fun coord -> coord |> fst = f)
            | RankHint r -> (fun coord -> coord |> snd = r)
            | SquareHint s -> (fun coord -> coord = s)
            | NoHint -> (fun _ -> true)
        candidates |> List.filter disambiguator

    let addNotes (notes: Ending option) (capture: SanCapture option) warnings moveInfo : SanMove =
        match moveInfo with
        | LegalMove m ->
            let mutable warnings = warnings
            let warn w = warnings <- w :: warnings

            let checkNote = notes = Some(SanCheck)
            let checkReal = m.ResultPosition.Observations |> MyList.contains Check
            if not checkNote && checkReal then warn IsCheck
            else if checkNote && not checkReal then warn IsNotCheck
                    
            let mateNote = notes = Some(SanMate)
            let mateReal = m.ResultPosition.Observations |> MyList.contains Mate
            if not mateNote && mateReal then warn IsMate
            else if mateNote && not mateReal then warn IsNotMate
                    
            let captureNote = capture = Some(SanCapture)
            let captureReal = m.Observations |> MyList.contains Capture
            if not captureNote && captureReal then warn IsCapture
            else if captureNote && not captureReal then warn IsNotCapture
            
            Interpreted(moveInfo, warnings)
        | IllegalMove _ -> Interpreted(moveInfo, [])

    let castling opt notes = 
        let move = 
            match color, opt with
            | White, ShortCastling -> "e1-g1"
            | White, LongCastling -> "e1-c1"
            | Black, ShortCastling -> "e8-g8"
            | Black, LongCastling -> "e8-c8"
            | _ -> failwith "unexpected"
        board 
        |> ValidateMove(_cn (move))
        |> addNotes notes None []
    
    let validateUsual fromSquare toSquare = 
        ValidateMove (UsualMove (fromSquare, toSquare))

    let validate promoteTo fromSquare toSquare = 
        match promoteTo with
        | Some p -> 
            let m = { Vector = (fromSquare, toSquare)
                      PromoteTo = p }
            ValidateMove (PromotionMove m)
        | None -> validateUsual fromSquare toSquare

    let interpret validate fromSquare toSquare notes capture warnings = 
        board 
        |> validate fromSquare toSquare
        |> addNotes notes capture warnings

    let toSanMove find validate hint pieceType toSquare notes capture = 
        let candidates = find (toSquare |> toX88)
        match candidates |> disambiguate hint with
        | [] -> Nonsense (PieceNotFound (color, pieceType))
        | fromSquare::[] -> 
            let warnings = 
                if candidates.Length = 1 && hint <> NoHint then 
                    [ DisambiguationIsExcessive ]
                else []
            interpret validate fromSquare toSquare notes capture warnings
        | filtered -> Nonsense (AmbiguousChoice filtered)

    let convert = function 
        | ShortCastling, notes -> castling ShortCastling notes
        | LongCastling, notes -> castling LongCastling notes
        | PawnPush(toSquare, promoteTo), notes -> 
            toSanMove findPushingPawns (validate promoteTo) 
                NoHint Pawn toSquare notes None
        | PawnCapture(fromFile, (toSquare, promoteTo)), notes -> 
            toSanMove findCapturingPawns (validate promoteTo) 
                (FileHint fromFile) Pawn toSquare notes (Some(SanCapture))
        | Usual(pieceType, (hint, (capture, toSquare))), notes -> 
            toSanMove (findNonPawnPieces pieceType) validateUsual 
                hint pieceType toSquare notes capture
           
    match ParseSanString str with
    | Success(p, _, _) -> Result.Ok(convert p)
    | Failure(e, _, _) -> Result.Error(e)
