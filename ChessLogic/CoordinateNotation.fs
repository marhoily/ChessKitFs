module CoordinateNotation

open Definitions
open FParsec
open Parsing
open System

let ToIndex = function 
    | (file, rank) -> rank * 8 + file
let PieceAt coordinate position = position.Placement.[ToIndex coordinate]
let vectorToString = function 
    | (f, t) -> CoordinateToString f + "-" + CoordinateToString t

type PromotionMove = 
    { Vector : Coordinate * Coordinate
      PromoteTo : PieceType }

[<StructuredFormatDisplay("{AsString}")>]
type Move = 
    | UsualMove of Coordinate * Coordinate
    | PromotionMove of PromotionMove
    member this.AsString = 
        match this with
        | UsualMove(f, t) -> vectorToString (f, t)
        | PromotionMove move -> 
            String.Format
                ("{0}={1}", vectorToString move.Vector, 
                 PieceToString(White, move.PromoteTo))

let ToCoordinateNotation (m:Move) = m.AsString

let private coordinate = 
    pipe2 (anyOf "abcdefgh") pint32 
        (fun file rank -> (LetterToFileNoCheck file, 8 - rank))
let ParseCoordinate str = wrap (run coordinate str)
let _c = ParseCoordinate >> unwrap

let ParseCoordinateNotation str = 
    let factory c1 c2 promotion = 
        match promotion with
        | Some(pieceType) -> 
            PromotionMove({ Vector = (c1, c2)
                            PromoteTo = parsePromotionHint pieceType})
        | None -> UsualMove(c1, c2)
    
    let c1 = coordinate .>> ((pchar '-') <|> (pchar 'x'))
    let p = opt ((pchar '=') >>. (anyOf "NBRQK"))
    let notation = pipe3 c1 coordinate p factory
    wrap (run notation str)

let _cn = ParseCoordinateNotation >> unwrap
