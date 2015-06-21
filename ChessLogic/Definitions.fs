﻿namespace ChessKit.ChessLogic

open System.Runtime.CompilerServices
open System
open Operators

type File = int

type Rank = int

/// Represents chess piece/player's side color
type Color = 
    | Black       = 0b01000000
    | White       = 0b10000000

/// Represents different piece types
type PieceType = 
    | None        = 0b00000000
    | Pawn        = 0b00000001
    | Knight      = 0b00000010
    | Bishop      = 0b00000100
    | Rook        = 0b00001000
    | Queen       = 0b00010000
    | King        = 0b00100000

/// Represents pieces on the board (color + type)
type Piece = 
    | EmptyCell = 0
    | WhitePawn   = 0b10000001
    | WhiteKnight = 0b10000010
    | WhiteBishop = 0b10000100
    | WhiteRook   = 0b10001000
    | WhiteQueen  = 0b10010000
    | WhiteKing   = 0b10100000
    | BlackPawn   = 0b01000001
    | BlackKnight = 0b01000010
    | BlackBishop = 0b01000100
    | BlackRook   = 0b01001000
    | BlackQueen  = 0b01010000
    | BlackKing   = 0b01100000

/// Represents different castling options that can be available to players
[<Flags>]
type Castlings = 
    | None = 0b0000
    | Q    = 0b0101
    | K    = 0b1010
    | W    = 0b0011
    | B    = 0b1100
    | WQ   = 0b0001
    | WK   = 0b0010
    | BQ   = 0b0100
    | BK   = 0b1000
    | All  = 0b1111

/// Represents different outcomes to the position a move can have
[<Flags>]
type MoveOutcomes = 
    | None                 = 0b00000000
    | Check                = 0b00000001
    | Mate                 = 0b00000010
    | Draw                 = 0b10000000
    | Repetition           = 0b10000100
    | FiftyMoveRule        = 0b10001000
    | Stalemate            = 0b10010000
    | InsufficientMaterial = 0b10100000

[<StructuredFormatDisplay("{AsString}")>]
type Move = 
    { FromIdx64 : int
      ToIdx64 : int
      PromoteTo : PieceType }

[<Flags>]
type MoveObservations = 
    | None       = 0b0000
    | Promotion  = 0b0001
    | Capture    = 0b0010
    | EnPassant  = 0b0100
    | DoublePush = 0b1000

[<Flags>]
type MoveWarnings = 
    | None                     = 0b00
    | MissingPromotionHint     = 0b01
    | PromotionHintIsNotNeeded = 0b10

[<Flags>]
type MoveErrors = 
    | None                  = 0b000000000000
    | MoveToCheck           = 0b000000000001
    | EmptyCell             = 0b000000000010
    | WrongSideToMove       = 0b000000000100
    | HasNoCastling         = 0b000000001000
    | ToOccupiedCell        = 0b000000010000
    | HasNoEnPassant        = 0b000000100000
    | DoesNotJump           = 0b000001000000
    | OnlyCapturesThisWay   = 0b000010000000
    | DoesNotCaptureThisWay = 0b000100000000
    | CastleThroughCheck    = 0b001000000000
    | DoesNotMoveThisWay    = 0b010000000000
    | CastleFromCheck       = 0b100000000000

type PositionCore = 
    { Placement : Piece array
      ActiveColor : Color
      CastlingAvailability : Castlings
      EnPassant : File option }

[<StructuredFormatDisplay("{AsString}")>]
type LegalMove = 
    { Move : Move
      OriginalPosition : Position
      ResultPosition : PositionCore
      Piece : PieceType
      Castling : Castlings
      Observations : MoveObservations
      Warnings : MoveWarnings }

and Position = 
    { Core : PositionCore
      HalfMoveClock : int
      FullMoveNumber : int
      Properties : MoveOutcomes
      Move : LegalMove option }

[<StructuredFormatDisplay("{AsString}")>]
[<NoComparison; NoEquality>]
type IllegalMove = 
    { Move : Move
      OriginalPosition : Position
      Piece : PieceType
      Castling : Castlings
      Observations : MoveObservations
      Warnings : MoveWarnings
      Errors : MoveErrors }

[<NoComparison; NoEquality>]
type MoveInfo = 
    | LegalMove of LegalMove
    | IllegalMove of IllegalMove

// --------------------------------------------------
module Side = 
    let Invert = 
        function 
        | Color.White -> Color.Black
        | Color.Black -> Color.White
        | _ -> failwith "unexpected"

type Move with
    static member internal Create f t p = 
        { FromIdx64 = f
          ToIdx64 = t
          PromoteTo = p }

[<AutoOpen>]
module internal FlagsOperators = 
    let private colors = int (Color.White ||| Color.Black)
    let (+|+) color pieceType = enum<Piece> (int (color) ||| int (pieceType))
    let getColor (piece : Piece) = enum<Color> (int (piece) &&& colors)
    let getPieceType (piece : Piece) = 
        enum<PieceType> (int (piece) &&& ~~~colors)

module internal Text = 
    let parseFile (p : char) : File = int (p) - int ('a')
    let fileToStirng (f : File) = char (int 'a' + f) |> string
    let rankToString (rank : Rank) = string (8 - rank)
    
    let pieceToChar = 
        function 
        | Piece.WhitePawn -> 'P'
        | Piece.WhiteKnight -> 'N'
        | Piece.WhiteBishop -> 'B'
        | Piece.WhiteRook -> 'R'
        | Piece.WhiteQueen -> 'Q'
        | Piece.WhiteKing -> 'K'
        | Piece.BlackPawn -> 'p'
        | Piece.BlackKnight -> 'n'
        | Piece.BlackBishop -> 'b'
        | Piece.BlackRook -> 'r'
        | Piece.BlackQueen -> 'q'
        | Piece.BlackKing -> 'k'
        | Piece.EmptyCell -> ' '
        | _ -> failwith "Unexpected"

open Text
open FParsec

[<RequireQualifiedAccess>]
module Coordinate = 
    let internal parser = 
        let parseFile (p : char) : File = int (p) - int ('a')
        let parseRank c = 8 - (int c - int '0')
        let file = anyOf "abcdefgh" |>> parseFile
        let rank = anyOf "12345678" |>> parseRank
        file .>>. rank
    
    let TryParse(str : string) = run parser str
    let Parse(str : string) = TryParse str |> Operators.getSuccess
    let FromIdx64 i = (i % 8, i / 8)
    
    let At coordinate position = 
        let toIdx64 (file, rank) = file + rank * 8
        position.Placement.[toIdx64 coordinate]
    
    let ToString(file, rank) = fileToStirng file + rankToString rank

[<RequireQualifiedAccess>]
module Idx64 = 
    let GetColor(idx64 : int) = 
        let file, rank = Coordinate.FromIdx64 idx64
        if (file % 2) = (rank % 2) then Color.White
        else Color.Black
    
    let FromCoordinate(file, rank) = file + rank * 8
    let internal parser = Coordinate.parser |>> FromCoordinate
    let internal fromX88 i = i % 16 + (i / 16) * 8
    let Rank(idx64 : int) = idx64 / 8
    let File(idx64 : int) = idx64 % 8
    let TryParse(str : string) = run parser str
    let Parse(str : string) = TryParse str |> Operators.getSuccess
    let ToString(idx64 : int) = 
        Coordinate.ToString (idx64 |> Coordinate.FromIdx64)

/// https://chessprogramming.wikispaces.com/0x88
[<RequireQualifiedAccess>]
module internal X88 = 
    let fromIdx64 i = i % 8 + (i / 8) * 16
    let fromCoordinate (file, rank) = file + rank * 16
    let parse = Coordinate.Parse >> fromCoordinate
    let at cX88 position = position.Placement.[Idx64.fromX88 cX88]

type Move with
    
    static member internal toString this = 
        let toStr = Coordinate.FromIdx64 >> Coordinate.ToString
        let vectorToString (f, t) = toStr f + "-" + toStr t
        let vector = vectorToString (this.FromIdx64, this.ToIdx64)
        if this.PromoteTo = PieceType.None then vector
        else 
            let p = pieceToChar (Color.White +|+ this.PromoteTo)
            sprintf "%s=%c" vector p
    
    member internal this.AsString = Move.toString this
    
    static member TryParse(str : string) = 
        let parsePromotionHint = 
            function 
            | 'N' -> PieceType.Knight
            | 'B' -> PieceType.Bishop
            | 'R' -> PieceType.Rook
            | 'Q' -> PieceType.Queen
            | _ -> failwith ("unknown promotion hint")
        
        let f = Idx64.parser .>> (pchar '-' <|> pchar 'x')
        let hint = anyOf "NBRQK" |>> parsePromotionHint
        let p = opt (pchar '=' >>. hint) |>> (fun x -> x ?|? PieceType.None)
        let notation = pipe3 f Idx64.parser p (Move.Create)
        run notation str
    
    static member Parse(str : string) = 
        Move.TryParse str |> Operators.getSuccess

type LegalMove with
    member internal x.AsString = x.Move.AsString

type IllegalMove with
    member internal x.AsString = 
        let errors = x.Errors |> sprintf "%A"
        sprintf "%s (%s)" x.Move.AsString errors

[<Extension>]
module BoardTextExtensions = 
    [<Extension>]
    let Dump board = 
        let sb = 
            System.Text.StringBuilder
                (" ╔═══╤═══╤═══╤═══╤═══╤═══╤═══╤═══╗\r\n" 
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
        for index = 0 to 63 do
            let piece = board.Core.Placement.[index]
            let file, rank = Coordinate.FromIdx64 index
            let i = (rank * 2 + 1) * 36 + file * 4 + 3
            sb.[i] <- pieceToChar piece
        string sb

module internal PositionCoreExt = 
    type PositionCore with
        member this.atIdx64 c64 = this.Placement.[c64]
        member this.atX88 cX88 = X88.at cX88 this
        member this.atStr = Idx64.Parse >> this.atIdx64
