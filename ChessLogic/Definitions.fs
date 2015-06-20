namespace ChessKit.ChessLogic

open System.Runtime.CompilerServices

type File = int

type Rank = int

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
    { Start : File * Rank
      End : File * Rank
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

// --------------------------------------------------
type Color with
    member this.Invert = 
        match this with
        | White -> Black
        | Black -> White

type Move with
    static member internal Create f t p = 
        { Start = f
          End = t
          PromoteTo = p }

module internal Text = 
    open Microsoft.FSharp.Reflection
    
    let parseFile (p : char) : File = int (p) - int ('a')
    let fileToStirng (f : File) = char (int 'a' + f) |> string
    let rankToString (rank : Rank) = string (8 - rank)
    let squareToString = function 
        | (file, rank) -> fileToStirng file + rankToString rank
    
    let pieceToChar = 
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
    
    let fieldName (x : 'a) = 
        match FSharpValue.GetUnionFields(x, typeof<'a>) with
        | case, _ -> case.Name
    
    let concatFieldNames sep list = 
        String.concat sep (list |> List.map fieldName)
    
    let moveToString this = 
        let vectorToString = function 
            | (f, t) -> squareToString f + "-" + squareToString t
        let vector = vectorToString (this.Start, this.End)
        if this.PromoteTo = None then vector
        else 
            let p = pieceToChar (White, this.PromoteTo.Value)
            sprintf "%s=%c" vector p

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

type Move with
    member internal this.AsString = moveToString this
    
    static member TryParse(str : string) = 
        let parsePromotionHint = 
            function 
            | 'N' -> Knight
            | 'B' -> Bishop
            | 'R' -> Rook
            | 'Q' -> Queen
            | _ -> failwith ("unknown promotion hint")
        
        let f = Coordinate.parser .>> (pchar '-' <|> pchar 'x')
        let hint = anyOf "NBRQK" |>> parsePromotionHint
        let p = opt (pchar '=' >>. hint)
        let notation = pipe3 f Coordinate.parser p (Move.Create)
        run notation str
    
    static member Parse(str : string) = 
        Move.TryParse str |> Operators.getSuccess

type LegalMove with
    member internal x.AsString = x.Move.AsString

type IllegalMove with
    member internal x.AsString = 
        let errors = x.Errors |> List.map fieldName
        sprintf "%s (%s)" x.Move.AsString (String.concat ", " errors)

[<Extension>]
module Extensions = 
    open System.Text
    
    [<Extension>]
    let Dump board = 
        let sb = 
            StringBuilder
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
        for position = 0 to 63 do
            let piece = board.Core.Placement.[position]
            let file = position % 8
            let rank = position / 8
            let index = (rank * 2 + 1) * 36 + file * 4 + 3
            sb.[index] <- (match piece with
                           | None -> ' '
                           | Some(p) -> pieceToChar p)
        string sb

module internal X88 = 
    // https://chessprogramming.wikispaces.com/0x88
    let toX88 = function 
        | (x, y) -> x + y * 16
    let fromX88 i = (i % 16, i / 16)
    let from64 i = (i % 8, i / 8)
    let to64 = function 
        | (file, rank) -> rank * 8 + file
    let PieceAt coordinate position = position.Placement.[to64 coordinate]
    
    type PositionCore with
        member this.at c = this.Placement.[c |> to64]
        member this.at64 c64 = this.Placement.[c64]
        member this.atX88 cX88 = this.Placement.[cX88
                                                 |> fromX88
                                                 |> to64]
