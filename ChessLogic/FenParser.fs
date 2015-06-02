module FenParser

open Definitions
open Parsing
open FParsec

type private Code = 
    | Piece of char
    | Gap of int

let ParseFen str = 
    let square = (anyOf "pnbrqkPNBRQK" |>> Piece) <|> (pint32 |>> Gap)
    let rank = many1 square
    let piecePlacement = sepBy rank (pchar '/')
    
    let activeColor = 
        anyOf "bw" |>> (function 
        | 'b' -> Black
        | _ -> White)
    
    let ws = pchar ' '
    let castlingAvailability = 
        (pchar '-' |>> (fun _ -> [])) <|> (many (anyOf "KQkq") |>> id)
    let enPassant = 
        (pchar '-' |>> (fun _ -> None)) 
        <|> ((anyOf "abcdefgh" .>>. anyOf "36") |>> Some)
    let n = pint32
    let theRest = 
        tuple5 (activeColor .>> ws) (castlingAvailability .>> ws) 
            (enPassant .>> ws) (n .>> ws) n
    
    let parsePlacement p = 
        [| for rank in p do
               for code in rank do
                   match code with
                   | Piece(p) -> yield Some(parsePieceLetter (p))
                   | Gap(n) -> 
                       for i = 1 to n do
                           yield None |]
    
    let parseEnPassant = 
        function 
        | Some(p, _) -> Some(LetterToFileNoCheck p)
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
