module ChessKit.ChessLogic.FenParser

open Text
open Parsing
open FParsec

type private Code = 
    | Piece of Piece
    | Gap of int

let ParseFen str = 
    let parseGap c = Gap(int c - int '0')
    let parsePiece c = Piece(parsePieceLetter c)
    
    let parsePlacement ranks = 
        [| for rank in ranks do
               for square in rank do
                   match square with
                   | Piece(p) -> yield Some(p)
                   | Gap(n) -> 
                       for _ in 1..n -> None |]
    
    let parseEnPassant = 
        function 
        | Some(p, _) -> Some(p)
        | None -> None
    
    let createCore plcmnt clr ca enp = 
        { Placement = parsePlacement (plcmnt)
          ActiveColor = clr
          CastlingAvailability = ca
          EnPassant = parseEnPassant (enp) }
    
    let createPosition core halfMoveClock fullMoveNumber = 
        { Core = core
          HalfMoveClock = halfMoveClock
          FullMoveNumber = fullMoveNumber
          Observations = []
          Move = None }
    
    let piece = anyOf "pnbrqkPNBRQK" |>> parsePiece <?> "piece symbol"
    let gap = anyOf "12345678" |>> parseGap <?> "number 1..8"
    let rank = many1 (piece <|> gap)
    let ws = pchar ' '
    let ranks = sepBy1 rank (pchar '/') .>> ws
    let black = pchar 'b' >>% Black
    let white = pchar 'w' >>% White
    let color = black <|> white .>> ws
    let castlingHint = anyOf "KQkq" |>> CastlingHint.parse
    let noCastling = pchar '-' >>% []
    let ca = noCastling <|> many1 castlingHint .>> ws
    let file = anyOf "abcdefgh" |>> LetterToFileNoCheck
    let enColor = (pchar '3' >>% Black) <|> (pchar '6' >>% White)
    let en = (file .>>. enColor) |>> Some
    let enPassant = ((pchar '-' >>% None) <|> en) .>> ws
    let core = pipe4 ranks color ca enPassant createCore
    let n = pint32
    let fenParser = pipe3 core (n .>> ws) n createPosition
    run fenParser str

let StartingPosition = 
    ParseFen "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1" 
    |> Operators.getSuccess
