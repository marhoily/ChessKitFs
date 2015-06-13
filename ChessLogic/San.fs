module San

open Definitions
open MoveLegalityChecker
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
    let isSimilarTo (a:LegalMove) (b:LegalMove) = 
        let x, y = a.Move, b.Move
        (x.Start <> y.Start) && (x.End = y.End) && (at x.Start = at y.Start)
    
    let disambiguationList = 
        lazy ([ for m in move.OriginalPosition |> GetLegalMoves.All do
                    if m |> isSimilarTo move then yield m.Move.Start ])
    let m = move.Move
    let ambiguous() = not (disambiguationList.Value |> List.isEmpty)
    let unique fn = 
        disambiguationList.Value 
        |> List.forall (fun x -> (m.Start |> fn) <> (x |> fn))
    // Actual algorithm
    if shortCastling then append "O-O"
    else if longCastling then append "O-O-O"
    else 
        if move.Piece = Pawn then 
            if capture then append (fileStr m.Start)
        else 
            appendc (move.Piece |> typeToString)
            if ambiguous() then 
                if unique file then append (fileStr m.Start)
                else if unique rank then append (rankStr m.Start)
                else append (fileAndRankStr m.Start)
        if capture then appendc 'x'
        append (fileAndRankStr m.End)
    if promotion then 
        appendc '='
        appendc (m.PromoteTo.Value |> typeToString)
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
    | AmbiguousChoice of LegalMove list
    | ChoiceOfIllegalMoves of IllegalMove list

type SanWarning = 
    | IsCapture
    | IsNotCapture
    | IsCheck
    | IsNotCheck
    | IsMate
    | IsNotMate
    | DisambiguationIsExcessive

type SanMove = 
    | LegalSan of LegalMove * SanWarning list
    | IllegalSan of IllegalMove 
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
        let jump, _ = getScanners color at88 square
        match color with
        | Black -> jump Pawn [ -15; -17 ]
        | White -> jump Pawn [ +15; +17 ]
        |> project

    let findNonPawnPieces ofType square = 
        let jump, slide = getScanners color at88 square
        match ofType with
        | Knight -> jump Knight [ -33; -31; -18; -14; +33; +31; +18; +14 ]
        | Queen -> slide Queen [ +15; +17; -15; -17; +16; +01; -16; -01 ]
        | Rook -> slide Rook [ +16; +01; -16; -01 ]
        | Bishop -> slide Bishop [ +15; +17; -15; -17 ]
        | King -> jump King [ +15; +17; -15; -17; +16; +01; -16; -01 ]
        | Pawn -> failwith "unexpected"
        |> project
    (findPushingPawns, findCapturingPawns, findNonPawnPieces)

let FromSanString str board = 
    let color = board.ActiveColor
    let findPushingPawns, findCapturingPawns, findNonPawnPieces = 
        sanScanners board
    
    let addNotes (notes: Ending option) (capture: SanCapture option) warningsBefore m : SanMove =
        let warnings = ref warningsBefore
        let warn w = warnings := w :: !warnings

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
            
        LegalSan(m, !warnings)

    let addNotesToAny (notes: Ending option) (capture: SanCapture option) warningsBefore moveInfo : SanMove =
        match moveInfo with
        | LegalMove m -> addNotes notes capture warningsBefore m
        | IllegalMove m -> IllegalSan m

    let castlingToSanMove opt notes = 
        let move = 
            match color, opt with
            | White, ShortCastling -> "e1-g1"
            | White, LongCastling -> "e1-c1"
            | Black, ShortCastling -> "e8-g8"
            | Black, LongCastling -> "e8-c8"
            | _ -> failwith "unexpected"
        board 
        |> ValidateMove(_cn (move))
        |> addNotesToAny notes None []
    
    let validate promoteTo fromSquare toSquare = 
        ValidateMove (Move.Create fromSquare toSquare promoteTo) board
    
    let toSanMove find validate hint pieceType toSquare notes capture = 
        let myPartition l =
            let mutable valid = []
            let mutable invalid = []
            for i in l do
                match i with
                | LegalMove m -> valid <- m::valid
                | IllegalMove m -> invalid <- m::invalid
            (valid, invalid)
        let validCandidates, invalidCandidates = 
            find (toSquare |> toX88)
            |> List.map (fun x -> validate x toSquare)
            |> myPartition

        let disambiguator (m : IMoveSource) = 
            let start = m.Move.Start
            match hint with
            | FileHint f -> start |> fst = f
            | RankHint r -> start |> snd = r
            | SquareHint s -> start = s
            | NoHint -> true
        let disambiguate moves = 
            moves |> List.filter disambiguator
        let valid = disambiguate validCandidates
        let invalid = disambiguate invalidCandidates

        let warnings = 
            if validCandidates.Length = 1 && hint <> NoHint then 
                [ DisambiguationIsExcessive ]
            else []
        match valid, invalid with
        | [], _::_::[] -> Nonsense (ChoiceOfIllegalMoves invalid)
        | [], only::[] -> IllegalSan only
        | [], [] -> Nonsense (PieceNotFound (color, pieceType))
        | validMove::[], _ -> validMove |> addNotes notes capture warnings
        | tooMany, _ -> Nonsense (AmbiguousChoice tooMany)
 
    let dispatch = function 
        | ShortCastling, notes -> castlingToSanMove ShortCastling notes
        | LongCastling, notes -> castlingToSanMove LongCastling notes
        | PawnPush(toSquare, promoteTo), notes -> 
            toSanMove findPushingPawns (validate promoteTo) 
                NoHint Pawn toSquare notes None
        | PawnCapture(fromFile, (toSquare, promoteTo)), notes -> 
            toSanMove findCapturingPawns (validate promoteTo) 
                (FileHint fromFile) Pawn toSquare notes (Some(SanCapture))
        | Usual(pieceType, (hint, (capture, toSquare))), notes -> 
            toSanMove (findNonPawnPieces pieceType) (validate None) 
                hint pieceType toSquare notes capture
           
    match ParseSanString str with
    | Success(p, _, _) -> dispatch p
    | Failure(e, _, _) -> Unparsable e

let FromLegalSanString str board = 
    match FromSanString str board with
    | LegalSan (move, _) -> move
    | x -> failwithf "%A" x
