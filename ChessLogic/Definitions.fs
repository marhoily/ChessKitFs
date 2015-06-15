﻿module Definitions

open System.Text
open Microsoft.FSharp.Reflection

type File = int

let fileToStirng (f : File) = char (int 'a' + f) |> string
let LetterToFileNoCheck(p : char) : File = int (p) - int ('a')

type Rank = int

let rankToString (rank : Rank) = string (8 - rank)

type Coordinate = File * Rank

// https://chessprogramming.wikispaces.com/0x88
let toX88 = function 
    | (x, y) -> x + y * 16
let fromX88 i = (i % 16, i / 16)
let CoordinateToString = function 
    | (file, rank) -> fileToStirng file + rankToString rank

type Color = 
    | Black
    | White
    
    override this.ToString() = 
        match this with 
        | White -> "w"
        | Black -> "b"
    
    static member OppositeOf = 
        function 
        | White -> Black
        | Black -> White
    
type PieceType = 
    | Pawn
    | Knight
    | Bishop
    | Rook
    | Queen
    | King

type Piece = Color * PieceType

let PieceToString = 
    function 
    | (White, Pawn) -> 'P'
    | (White, Knight) -> 'N'
    | (White, Bishop) -> 'B'
    | (White, Rook) -> 'R'
    | (White, Queen) -> 'Q'
    | (White, King) -> 'K'
    | (Black, Pawn) -> 'p'
    | (Black, Knight) -> 'n'
    | (Black, Bishop) -> 'b'
    | (Black, Rook) -> 'r'
    | (Black, Queen) -> 'q'
    | (Black, King) -> 'k'

type CastlingHint = 
    | WQ
    | WK
    | BQ
    | BK
    
    static member toString = 
        function 
        | WQ -> 'Q'
        | WK -> 'K'
        | BQ -> 'q'
        | BK -> 'k'
    
    static member parse = 
        function 
        | 'Q' -> WQ
        | 'K' -> WK
        | 'q' -> BQ
        | 'k' -> BK
        | _ -> failwith "unknown castling symbol"

type PositionObservation = 
    | Check
    | Mate
    | Repetition
    | FiftyMoveRule
    | Stalemate
    | InsufficientMaterial

let vectorToString = function 
    | (f, t) -> CoordinateToString f + "-" + CoordinateToString t

[<StructuredFormatDisplay("{AsString}")>]
type Move = 
    { Start : Coordinate
      End : Coordinate
      PromoteTo : PieceType option }
    
    member this.AsString = 
        let vector = vectorToString (this.Start, this.End)
        if this.PromoteTo = None then vector
        else 
            let p = PieceToString(White, this.PromoteTo.Value)
            sprintf "%s=%c" vector p
    
    static member Create f t p = 
        { Start = f
          End = t
          PromoteTo = p }

type Observation = 
    | Capture
    | EnPassant
    | Promotion
    | DoublePush

type Warning = 
    | MissingPromotionHint
    | PromotionHintIsNotNeeded

type Error = 
    | MoveToCheck
    | EmptyCell
    | WrongSideToMove
    | HasNoCastling
    | ToOccupiedCell
    | HasNoEnPassant
    | DoesNotJump
    | OnlyCapturesThisWay
    | DoesNotCaptureThisWay
    | CastleThroughCheck
    | DoesNotMoveThisWay
    | CastleFromCheck

let toString (x : 'a) = 
    match FSharpValue.GetUnionFields(x, typeof<'a>) with
    | case, _ -> case.Name

let listToString sep list = 
    let strings = list |> List.map toString
    String.concat sep strings

type PositionCore = 
    { Placement : Piece option array
      ActiveColor : Color
      CastlingAvailability : CastlingHint list
      EnPassant : File option }

type Position = 
    { Core : PositionCore
      HalfMoveClock : int
      FullMoveNumber : int
      Observations : PositionObservation list
      Move : LegalMove option }
    static member FromCore core = 
        { Core = core
          Move = None
          HalfMoveClock = 0
          FullMoveNumber = 0
          Observations = [] }
    static member FromCoreAndMove core move = 
        { Core = core
          Move = Some(move)
          HalfMoveClock = 0
          FullMoveNumber = 0
          Observations = [] }
    override x.ToString() = 
        let errors = x.Observations |> List.map toString
        sprintf " (%s)" (String.concat ", " errors)

and 
    [<StructuredFormatDisplay("{AsString}")>]
    LegalMove = 
    { Move : Move
      OriginalPosition : Position
      ResultPosition : PositionCore
      Piece : PieceType
      Castling : CastlingHint option
      Observations : Observation list
      Warnings : Warning list }
    member x.AsString = x.Move.AsString

[<StructuredFormatDisplay("{AsString}")>]
type IllegalMove = 
    { Move : Move
      OriginalPosition : Position
      Piece : PieceType option
      Castling : CastlingHint option
      Observations : Observation list
      Warnings : Warning list
      Errors : Error list }
    member x.AsString = 
        let errors = x.Errors |> List.map toString
        sprintf "%s (%s)" x.Move.AsString (String.concat ", " errors)

let EmptyPosition = 
    { Core = 
          { Placement = [||]
            ActiveColor = White
            CastlingAvailability = [ WK; WQ; BK; BQ ]
            EnPassant = None }
      HalfMoveClock = 0
      FullMoveNumber = 1
      Observations = []
      Move = None }

let inline (?|?) a b = 
    match a with
    | Some(x) -> x
    | None -> b