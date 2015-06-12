module FenParser

open Definitions
open Parsing
open FParsec

type private Code = 
    | Piece of Piece
    | Gap of int

let ParseFen str = 
    let piece = anyOf "pnbrqkPNBRQK" |>> (fun c -> Piece(parsePieceLetter c)) <?> "piece symbol"
    let gap = anyOf "12345678" |>> (fun c -> Gap(int c - int '0')) <?> "number 1..8"
    let rank = many1 (piece <|> gap)
    let piecePlacement = sepBy1 rank (pchar '/')
    
    let black = pchar 'b' >>% Black
    let white = pchar 'w' >>% White
    let activeColor = white <|> black
    
    let ws = pchar ' '
    let castlingAvailability = 
        (pchar '-' >>% []) <|> (many (anyOf "KQkq") |>> id)
    let file = anyOf "abcdefgh" |>> LetterToFileNoCheck
    let enPassant = 
        (pchar '-' >>% None)
        <|> ((file .>>. anyOf "36") |>> Some)
    let n = pint32
    let theRest = 
        tuple5 (activeColor .>> ws) (castlingAvailability .>> ws) 
            (enPassant .>> ws) (n .>> ws) n
    
    let parsePlacement p = 
        [| for rank in p do
               for code in rank do
                   match code with
                   | Piece(p) -> yield Some(p)
                   | Gap(n) -> for _ in 1..n -> None |]
    
    let parseEnPassant = 
        function 
        | Some(p, _) -> Some(p)
        | None -> None
    
    let fenParser = 
        pipe2 (piecePlacement .>> ws) theRest (fun placement (activeColor, castlingAvailability, enPassant, halfMoveClock, fullMoveNumber) -> 
            { Placement = parsePlacement (placement)
              ActiveColor = activeColor
              CastlingAvailability = 
                  castlingAvailability |> List.map CastlingHint.parse
              EnPassant = parseEnPassant (enPassant)
              HalfMoveClock = halfMoveClock
              FullMoveNumber = fullMoveNumber 
              Observations = []})
    
    wrap (run fenParser str)

let StartingPosition = 
    ParseFen "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1" |> function 
    | Result.Ok(position) -> position
    | Result.Error(error) -> failwith error
