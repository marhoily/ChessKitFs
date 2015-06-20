namespace ChessKit.ChessLogic

open System.Runtime.CompilerServices
open System
open Operators

type File = int

type Rank = int

type Color = 
    | Black //= 1
    | White //= 2

type PieceType = 
    | None   = 0
    | Pawn   = 1
    | Knight = 2
    | Bishop = 3
    | Rook   = 4
    | Queen  = 5
    | King   = 6

type Piece = Color * PieceType

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

[<Flags>]
type Properties = 
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
    { Start : File * Rank
      End : File * Rank
      PromoteTo : PieceType }

[<Flags>]
type Observation = 
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
    { Placement : Piece option array
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
      Observations : Observation
      Warnings : MoveWarnings }

and Position = 
    { Core : PositionCore
      HalfMoveClock : int
      FullMoveNumber : int
      Properties : Properties
      Move : LegalMove option }

[<StructuredFormatDisplay("{AsString}")>]
type IllegalMove = 
    { Move : Move
      OriginalPosition : Position
      Piece : PieceType option
      Castling : Castlings
      Observations : Observation
      Warnings : MoveWarnings
      Errors : MoveErrors }

type MoveInfo = 
    | LegalMove of LegalMove
    | IllegalMove of IllegalMove

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
    
    let pieceToChar = 
        function 
        | (White, PieceType.Pawn) -> 'P'
        | (White, PieceType.Knight) -> 'N'
        | (White, PieceType.Bishop) -> 'B'
        | (White, PieceType.Rook) -> 'R'
        | (White, PieceType.Queen) -> 'Q'
        | (White, PieceType.King) -> 'K'
        | (Black, PieceType.Pawn) -> 'p'
        | (Black, PieceType.Knight) -> 'n'
        | (Black, PieceType.Bishop) -> 'b'
        | (Black, PieceType.Rook) -> 'r'
        | (Black, PieceType.Queen) -> 'q'
        | (Black, PieceType.King) -> 'k'
        | _ -> failwith "Unexpected"
    
    let fieldName (x : 'a) = 
        match FSharpValue.GetUnionFields(x, typeof<'a>) with
        | case, _ -> case.Name
    
    let concatFieldNames sep list = 
        String.concat sep (list |> List.map fieldName)

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
    let internal fromX88 i = (i % 16, i / 16)
    let internal fromIdx64 i = (i % 8, i / 8)
    let internal toIdx64 (file, rank) = rank * 8 + file
    let At coordinate position = position.Placement.[toIdx64 coordinate]
    let ToString (file, rank) = fileToStirng file + rankToString rank

[<RequireQualifiedAccess>]
module Idx64 = 
    let GetColor(c : int) = 
        let file, rank = c % 8, c / 8
        if (file % 2) = (rank % 2) then White
        else Black

type Move with
    
    static member internal toString this = 
        let vectorToString (f, t) = 
            Coordinate.ToString f + "-" + Coordinate.ToString t
        let vector = vectorToString (this.Start, this.End)
        if this.PromoteTo = PieceType.None then vector
        else 
            let p = pieceToChar (White, this.PromoteTo)
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
        
        let f = Coordinate.parser .>> (pchar '-' <|> pchar 'x')
        let hint = anyOf "NBRQK" |>> parsePromotionHint
        let p = opt (pchar '=' >>. hint) |>> (fun x -> x ?|? PieceType.None)
        let notation = pipe3 f Coordinate.parser p (Move.Create)
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
    open System.Text
    
    [<Extension>]
    let Dump board = 
        let sb = 
            StringBuilder
                (  " ╔═══╤═══╤═══╤═══╤═══╤═══╤═══╤═══╗\r\n" 
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
            let file, rank = Coordinate.fromIdx64 index
            let i = (rank * 2 + 1) * 36 + file * 4 + 3
            sb.[i] <- (match piece with
                       | None -> ' '
                       | Some(p) -> pieceToChar p)
        string sb

/// https://chessprogramming.wikispaces.com/0x88
[<RequireQualifiedAccess>]
module internal X88 = 
    let fromCoordinate (x, y) = x + y * 16
    let parse = Coordinate.Parse >> fromCoordinate
    let toIdx64 = Coordinate.fromX88 >> Coordinate.toIdx64
    let at cX88 position = position.Placement.[toIdx64 cX88]
    let fromIdx64 i = (i % 8, i / 8) |> fromCoordinate

module internal PositionCoreExt = 
    type PositionCore with
        member this.at c = Coordinate.At c this
        member this.atIdx64 c64 = this.Placement.[c64]
        member this.atX88 cX88 = X88.at cX88 this
        member this.atStr = Coordinate.Parse >> this.at
