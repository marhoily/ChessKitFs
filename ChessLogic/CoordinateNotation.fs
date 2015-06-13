module CoordinateNotation

open Definitions
open FParsec
open Parsing

let ToIndex = function 
    | (file, rank) -> rank * 8 + file
let PieceAt coordinate position = position.Placement.[ToIndex coordinate]



let ToCoordinateNotation (m:Move) = m.AsString

let private coordinate = 
    pipe2 (anyOf "abcdefgh") pint32 
        (fun file rank -> (LetterToFileNoCheck file, 8 - rank))
let ParseCoordinate str = wrap (run coordinate str)
let _c = ParseCoordinate >> unwrap

let ParseCoordinateNotation str = 
    let f = coordinate .>> ((pchar '-') <|> (pchar 'x'))
    let hint = anyOf "NBRQK" |>> parsePromotionHint
    let p = opt ((pchar '=') >>. hint) 
    let notation = pipe3 f coordinate p (Move.Create)
    wrap (run notation str)

let _cn = ParseCoordinateNotation >> unwrap
