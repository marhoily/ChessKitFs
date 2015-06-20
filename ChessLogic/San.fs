[<RequireQualifiedAccess>]
module ChessKit.ChessLogic.San

open ChessKit.ChessLogic
open ChessKit.ChessLogic.Text
open ChessKit.ChessLogic.Scanning
open ChessKit.ChessLogic.PositionCoreExt
open System.Text
open FParsec
open ChessKit.ChessLogic
open Operators

let ToString(legalMove : LegalMove) = 
    //     _______________________
    // ___/ Shortcuts and helpers \___________________________
    let typeToString = 
        function 
        | Pawn -> 'P'
        | Knight -> 'N'
        | Bishop -> 'B'
        | Rook -> 'R'
        | Queen -> 'Q'
        | King -> 'K'
    
    let sb = new StringBuilder(6)
    let move = legalMove.Move
    let obs = (legalMove |> EndGame.ToPosition).Properties
    let shortCastling = legalMove.Castling = Some(WK) || legalMove.Castling = Some(BK)
    let longCastling = legalMove.Castling = Some(WQ) || legalMove.Castling = Some(BQ)
    let capture = legalMove.Observations |> List.contains Capture
    let promotion = legalMove.Observations |> List.contains Promotion
    let check = obs |> test Properties.Check
    let mate = obs |> test Properties.Mate
    let append (str : string) = sb.Append(str) |> ignore
    let appendc (str : char) = sb.Append(str) |> ignore
    let file, rank, fileAndRankStr = fst, snd, Coordinate.ToString
    let fileStr x = fileToStirng (x |> file)
    let rankStr x = rankToString (x |> rank)
    let at = legalMove.OriginalPosition.Core.at
    let isSimilarTo (a:LegalMove) (b:LegalMove) = 
        let x, y = a.Move, b.Move
        (x.Start <> y.Start) && (x.End = y.End) && (at x.Start = at y.Start)
    
    let disambiguationList = 
        lazy ([ for m in legalMove.OriginalPosition |> GetLegalMoves.All do
                    if m |> isSimilarTo legalMove then yield m.Move.Start ])
    let ambiguous() = not (disambiguationList.Value |> List.isEmpty)
    let unique fn = 
        disambiguationList.Value 
        |> List.forall (fun x -> (move.Start |> fn) <> (x |> fn))
    //     __________________
    // ___/ Actual algorithm \________________________________
    if shortCastling then append "O-O"
    else if longCastling then append "O-O-O"
    else 
        if legalMove.Piece = Pawn then 
            if capture then append (fileStr move.Start)
        else 
            appendc (legalMove.Piece |> typeToString)
            if ambiguous() then 
                if unique file then append (fileStr move.Start)
                else if unique rank then append (rankStr move.Start)
                else append (fileAndRankStr move.Start)
        if capture then appendc 'x'
        append (fileAndRankStr move.End)
    if promotion then 
        appendc '='
        appendc (move.PromoteTo.Value |> typeToString)
    if check then appendc '+'
    else if mate then appendc '#'
    string sb

type internal Ending = 
    | SanCheck
    | SanMate

type internal SanCapture = 
    | SanCapture

type internal Hint = 
    | FileHint of File
    | RankHint of Rank
    | SquareHint of (File * Rank)
    | NoHint

type internal Moves = 
    | ShortCastling
    | LongCastling
    | PawnPush of (File * Rank) * PieceType option
    | PawnCapture of File * ((File * Rank) * PieceType option)
    | Usual of (PieceType * (Hint * (SanCapture option * (File * Rank))))

let internal ParseSanString str = 
    let parseRank (c : char) : Rank = (int '8') - (int c)
    
    let parsePiece = 
        function 
        | 'N' -> Knight
        | 'B' -> Bishop
        | 'R' -> Rook
        | 'Q' -> Queen
        | 'K' -> King
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

type Error = 
    | PieceNotFound of Piece
    | AmbiguousChoice of LegalMove list
    | ChoiceOfIllegalMoves of IllegalMove list

type Warning = 
    | IsCapture
    | IsNotCapture
    | IsCheck
    | IsNotCheck
    | IsMate
    | IsNotMate
    | DisambiguationIsExcessive

type Move = 
    | Legal of LegalMove * Warning list
    | Illegal of IllegalMove
    | Nonsense of Error
    | Unparsable of string

let internal sanScanners board = 
    let color = board.ActiveColor
    let project = 
        Seq.map (fun f -> f())
        >> Seq.filter (fun x -> x <> -1)
        >> Seq.map Coordinate.fromX88
        >> Seq.toList

    let findPushingPawns square = 
        let _, slide = getScanners color board.atX88 square
        match color with
        | Black -> slide Pawn [ -16; ]
        | White -> slide Pawn [ +16; ]
        |> project

    let findCapturingPawns square = 
        let jump, _ = getScanners color board.atX88 square
        match color with
        | Black -> jump Pawn [ -15; -17 ]
        | White -> jump Pawn [ +15; +17 ]
        |> project

    let findNonPawnPieces ofType square = 
        let jump, slide = getScanners color board.atX88 square
        match ofType with
        | Knight -> jump Knight [ -33; -31; -18; -14; +33; +31; +18; +14 ]
        | Queen -> slide Queen [ +15; +17; -15; -17; +16; +01; -16; -01 ]
        | Rook -> slide Rook [ +16; +01; -16; -01 ]
        | Bishop -> slide Bishop [ +15; +17; -15; -17 ]
        | King -> jump King [ +15; +17; -15; -17; +16; +01; -16; -01 ]
        | Pawn -> failwith "unexpected"
        |> project
    (findPushingPawns, findCapturingPawns, findNonPawnPieces)

let TryParse str board = 
    let color = board.Core.ActiveColor
    let findPushingPawns, findCapturingPawns, findNonPawnPieces = 
        sanScanners board.Core
    
    let addNotesToLegal notes capture warns (legalMove:LegalMove) =
        let warnings = ref warns
        let warn w = warnings := w :: !warnings
        let obs = (legalMove |> EndGame.ToPosition).Properties
        
        let checkNote = notes = Some(SanCheck)
        let checkReal = obs |> test Properties.Check
        if not checkNote && checkReal then warn IsCheck
        else if checkNote && not checkReal then warn IsNotCheck
                    
        let mateNote = notes = Some(SanMate)
        let mateReal = obs |> test Properties.Mate
        if not mateNote && mateReal then warn IsMate
        else if mateNote && not mateReal then warn IsNotMate
                    
        let captureNote = capture = Some(SanCapture)
        let captureReal = legalMove.Observations |> List.contains Capture
        if not captureNote && captureReal then warn IsCapture
        else if captureNote && not captureReal then warn IsNotCapture
            
        Legal(legalMove, !warnings)

    let addNotesToAny notes capture warns moveInfo =
        match moveInfo with
        | LegalMove m -> addNotesToLegal notes capture warns m
        | IllegalMove m -> Illegal m

    let castlingToSanMove opt notes = 
        let move = 
            match color, opt with
            | White, ShortCastling -> "e1-g1"
            | White, LongCastling -> "e1-c1"
            | Black, ShortCastling -> "e8-g8"
            | Black, LongCastling -> "e8-c8"
            | _ -> failwith "unexpected"
        board 
        |> MoveLegality.Validate(Move.Parse (move))
        |> addNotesToAny notes None []
    
    let validate promoteTo fromSquare toSquare = 
        MoveLegality.Validate (Move.Create fromSquare toSquare promoteTo) board

    let legal (m : LegalMove) = m.Move.Start
    let illegal (m : IllegalMove) = m.Move.Start

    let disambiguate getStart hint moves = 
        let satisfyHint start = 
            match hint with
            | FileHint f -> start |> fst = f
            | RankHint r -> start |> snd = r
            | SquareHint s -> start = s
            | NoHint -> true
        
        moves |> List.filter (getStart >> satisfyHint)

    let findAndSeparate find toSquare validate () = 
        let separateToLegalAndIllegal list =
            let mutable valid = []
            let mutable invalid = []
            for move in list do
                match move with
                | MoveInfo.LegalMove m -> valid <- m::valid
                | IllegalMove m -> invalid <- m::invalid
            (valid, invalid)

        find (toSquare |> X88.fromCoordinate)
        |> List.map (fun x -> validate x toSquare)
        |> separateToLegalAndIllegal

    let toSanMove find hint pieceType addNotes = 
        let validCandidates, invalidCandidates = find()
        let valid = disambiguate legal hint validCandidates
        let invalid = disambiguate illegal hint invalidCandidates

        let warnings = 
            if validCandidates |> List.length = 1 && hint <> NoHint then 
                [ DisambiguationIsExcessive ]
            else []

        match valid, invalid with
        | [], _::_::[] -> Nonsense (ChoiceOfIllegalMoves invalid)
        | [], only::[] -> Illegal only
        | [], [] -> Nonsense (PieceNotFound (color, pieceType))
        | validMove::[], _ -> validMove |> addNotes warnings
        | tooMany, _ -> Nonsense (AmbiguousChoice tooMany)
 
    let dispatch = 
        function 
        | ShortCastling, notes -> castlingToSanMove ShortCastling notes
        | LongCastling, notes -> castlingToSanMove LongCastling notes
        | PawnPush(toSquare, promoteTo), notes -> 
            let addNotes = addNotesToLegal notes None
            let find = findAndSeparate findPushingPawns toSquare (validate promoteTo) 
            toSanMove find NoHint Pawn addNotes

        | PawnCapture(fromFile, (toSquare, promoteTo)), notes -> 
            let addNotes = addNotesToLegal notes (Some SanCapture)
            let find = findAndSeparate findCapturingPawns toSquare (validate promoteTo) 
            toSanMove find (FileHint fromFile) Pawn addNotes

        | Usual(pieceType, (hint, (capture, toSquare))), notes -> 
            let addNotes = addNotesToLegal notes capture
            let find = findAndSeparate (findNonPawnPieces pieceType) toSquare (validate None)
            toSanMove find hint pieceType addNotes
           
    match ParseSanString str with
    | Success(p, _, _) -> dispatch p
    | ParserResult.Failure(e, _, _) -> Unparsable e

let Parse str board = 
    match TryParse str board with
    | Legal (move, _) -> move
    | x -> failwithf "%A" x
