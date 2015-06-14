module FenParser

open Definitions
open Parsing
open FParsec

type private Code = 
    | Piece of Piece
    | Gap of int

let ParseFen str = 
    let parseGap c = Gap(int c - int '0')
    let parsePiece c = Piece(parsePieceLetter c)
    let piece = anyOf "pnbrqkPNBRQK" |>> parsePiece <?> "piece symbol"
    let gap = anyOf "12345678" |>> parseGap <?> "number 1..8"
    let rank = many1 (piece <|> gap)
    let piecePlacement = sepBy1 rank (pchar '/')
    let color = (pchar 'b' >>% Black) <|> (pchar 'w' >>% White)
    let castlingHint = anyOf "KQkq" |>> CastlingHint.parse
    let ws = pchar ' '
    let ca = (pchar '-' >>% []) <|> (many1 castlingHint)
    let file = anyOf "abcdefgh" |>> LetterToFileNoCheck
    let enColor = (pchar '3' >>% Black) <|> (pchar '6' >>% White)
    let en = (file .>>. enColor) |>> Some
    let enPassant = (pchar '-' >>% None) <|> en
    let n = pint32
    let theRest = 
        tuple5 (color .>> ws) (ca .>> ws) (enPassant .>> ws) (n .>> ws) n
    
    let parsePlacement p = 
        [| for rank in p do
               for code in rank do
                   match code with
                   | Piece(p) -> yield Some(p)
                   | Gap(n) -> 
                       for _ in 1..n -> None |]
    
    let parseEnPassant = 
        function 
        | Some(p, _) -> Some(p)
        | None -> None
    
    let fenParser = 
        pipe2 (piecePlacement .>> ws) theRest (fun placement (activeColor, castlingAvailability, enPassant, halfMoveClock, fullMoveNumber) -> 
            { Core = 
                { Placement = parsePlacement (placement)
                  ActiveColor = activeColor
                  CastlingAvailability = castlingAvailability
                  EnPassant = parseEnPassant (enPassant) }
              HalfMoveClock = halfMoveClock
              FullMoveNumber = fullMoveNumber
              Observations = []
              Move = None })
    
    wrap (run fenParser str)

let StartingPosition = 
    ParseFen "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1" |> function 
    | Result.Ok(position) -> position
    | Result.Error(error) -> failwith error
