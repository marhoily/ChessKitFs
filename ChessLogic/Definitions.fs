﻿namespace ChessKit.ChessLogic

type File = int

type Rank = int

type Coordinate = File * Rank

type Color = 
    | Black
    | White

type PieceType = 
    | Pawn
    | Knight
    | Bishop
    | Rook
    | Queen
    | King

type Piece = Color * PieceType

type CastlingHint = 
    | WQ
    | WK
    | BQ
    | BK

type PositionObservation = 
    | Check
    | Mate
    | Repetition
    | FiftyMoveRule
    | Stalemate
    | InsufficientMaterial

[<StructuredFormatDisplay("{AsString}")>]
type Move = 
    { Start : Coordinate
      End : Coordinate
      PromoteTo : PieceType option }

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

type PositionCore = 
    { Placement : Piece option array
      ActiveColor : Color
      CastlingAvailability : CastlingHint list
      EnPassant : File option }

[<StructuredFormatDisplay("{AsString}")>]
type LegalMove = 
    { Move : Move
      OriginalPosition : Position
      ResultPosition : PositionCore
      Piece : PieceType
      Castling : CastlingHint option
      Observations : Observation list
      Warnings : Warning list }

and Position = 
    { Core : PositionCore
      HalfMoveClock : int
      FullMoveNumber : int
      Observations : PositionObservation list
      Move : LegalMove option }

[<StructuredFormatDisplay("{AsString}")>]
type IllegalMove = 
    { Move : Move
      OriginalPosition : Position
      Piece : PieceType option
      Castling : CastlingHint option
      Observations : Observation list
      Warnings : Warning list
      Errors : Error list }

type Color with
    
    member this.AsString = 
        match this with
        | White -> "w"
        | Black -> "b"
    
    static member OppositeOf = 
        function 
        | White -> Black
        | Black -> White

type CastlingHint with
    
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

type Position with
    
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

type Move with
    static member internal Create f t p = 
        { Start = f
          End = t
          PromoteTo = p }

module X88 =
    // https://chessprogramming.wikispaces.com/0x88
    let toX88 = function 
        | (x, y) -> x + y * 16
    let fromX88 i = (i % 16, i / 16)

module Text =
    open System.Text
    open Microsoft.FSharp.Reflection

    let fileToStirng (f : File) = char (int 'a' + f) |> string
    let LetterToFileNoCheck(p : char) : File = int (p) - int ('a')
    let rankToString (rank : Rank) = string (8 - rank)
    let CoordinateToString = function 
        | (file, rank) -> fileToStirng file + rankToString rank

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

    let vectorToString = function 
        | (f, t) -> CoordinateToString f + "-" + CoordinateToString t

    let toString (x : 'a) = 
        match FSharpValue.GetUnionFields(x, typeof<'a>) with
        | case, _ -> case.Name

    let listToString sep list = 
        let strings = list |> List.map toString
        String.concat sep strings

    type Position with
        member board.Dump() = 
            let sb = 
                StringBuilder(" ╔═══╤═══╤═══╤═══╤═══╤═══╤═══╤═══╗\r\n" 
                            + "8║   │ r │   │   │ k │   │   │ r ║\r\n" 
                            + " ╟───┼───┼───┼───┼───┼───┼───┼───╢\r\n" 
                            + "7║ p │   │   │ n │   │ p │   │ p ║\r\n" 
                            + " ╟───┼───┼───┼───┼───┼───┼───┼───╢\r\n" 
                            + "6║ n │ p │   │   │   │   │ p │   ║\r\n" 
                            + " ╟───┼───┼───┼───┼───┼───┼───┼───╢\r\n" 
                            + "5║   │   │ p │   │ B │   │ b │   ║\r\n" 
                            + " ╟───┼───┼───┼───┼───┼───┼───┼───╢\r\n" 
                            + "4║   │   │   │   │   │   │   │ P ║\r\n" 
                            + " ╟───┼───┼───┼───┼───┼───┼───┼───╢\r\n" 
                            + "3║   │ P │   │ P │   │   │   │   ║\r\n" 
                            + " ╟───┼───┼───┼───┼───┼───┼───┼───╢\r\n" 
                            + "2║ P │   │ P │ N │   │   │   │ P ║\r\n" 
                            + " ╟───┼───┼───┼───┼───┼───┼───┼───╢\r\n" 
                            + "1║ R │ N │ Q │   │   │ R │ K │   ║\r\n" 
                            + " ╚═══╧═══╧═══╧═══╧═══╧═══╧═══╧═══╝\r\n" 
                            + "   A   B   C   D   E   F   G   H  \r\n")
            for position = 0 to 63 do
                let piece = board.Core.Placement.[position]
                let file = position % 8
                let rank = position / 8
                let index = (rank * 2 + 1) * 36 + file * 4 + 3
                sb.[index] <- (match piece with
                               | None -> ' '
                               | Some(p) -> PieceToString p)
            string sb

open Text

type Move with
    member this.AsString = 
        let vector = vectorToString (this.Start, this.End)
        if this.PromoteTo = None then vector
        else 
            let p = PieceToString(White, this.PromoteTo.Value)
            sprintf "%s=%c" vector p

type LegalMove with
    member x.AsString = x.Move.AsString

type IllegalMove with
    member x.AsString = 
        let errors = x.Errors |> List.map toString
        sprintf "%s (%s)" x.Move.AsString (String.concat ", " errors)

module Board =
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
