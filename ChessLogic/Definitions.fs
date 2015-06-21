namespace ChessKit.ChessLogic

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
    /// All queenside castlings
    | Q    = 0b0101
    /// All kingside castlings
    | K    = 0b1010
    /// All white's castlings
    | W    = 0b0011
    /// All black's castlings
    | B    = 0b1100
    /// Whites's queenside castling
    | WQ   = 0b0001
    /// Whites's kingside castling
    | WK   = 0b0010
    /// Black's queenside castling
    | BQ   = 0b0100
    /// Black's kingside castling
    | BK   = 0b1000
    /// All four existing castlings
    | All  = 0b1111

/// Represents different outcomes to the position a move can have
[<Flags>]
type MoveOutcomes =
    | None                 = 0b0000000
    /// The last move gives check
    | Check                = 0b0000001
    /// The last move gives check and mate
    | Mate                 = 0b0000010
    /// All existing draw outcomes
    | Draw                 = 0b1111111
    /// Draw by threefold repetition
    | Repetition           = 0b1000100
    /// Draw by not capturing or moving pawn for 50 moves
    | FiftyMoveRule        = 0b1001000
    /// Draw by not leaving the opponent any legal moves
    | Stalemate            = 0b1010000
    /// Draw by both sides not having enough material to give mate
    | InsufficientMaterial = 0b1100000

/// Represents a move as entered by user, prior to any legality checks
[<StructuredFormatDisplay("{AsString}")>]
type Move =
    { /// Square the move originates from (0..63)
      FromIdx64 : int
      /// Square the move destinates to (0..63)
      ToIdx64 : int
      /// Type of the piece the player whants to promote his pawn to
      PromoteTo : PieceType }

/// Annotations that can be made to the move during the legality check
[<Flags>]
type MoveAnnotations =
    | None       = 0b0000
    /// It was a pawn move to the last rank
    | Promotion  = 0b0001
    /// It captured the opponents piece
    | Capture    = 0b0010
    /// It was a pawn move to capture en passant
    | EnPassant  = 0b0100
    /// It was the pawn move 2 squares ahead from the original rank
    | DoublePush = 0b1000

/// Non-critical error that can be noticed about the move during
/// the legality check
[<Flags>]
type MoveWarnings =
    | None                     = 0b00
    /// It was annotated as Promotion but no PromoteTo
    /// piece type was assigned. Queens was used by default
    | MissingPromotionHint     = 0b01
    /// It was not a promotion move, but PromoteTo
    /// contained non-empty piece type, that was ignored
    | PromotionHintIsNotNeeded = 0b10

/// Critical errors that can be noticed about the move during
/// the legality check
[<Flags>]
type MoveErrors =
    | None                  = 0b000000000000
    /// Your king can be captured the next move
    | MoveToCheck           = 0b000000000001
    /// Your move originates in the empty cell
    | EmptyCell             = 0b000000000010
    /// It's not your turn to move (not your piece to move)
    | WrongSideToMove       = 0b000000000100
    /// You can't castle because the King or the Rook had moved
    | HasNoCastling         = 0b000000001000
    /// The move destinates in the cell occupied by the piece of your own
    | ToOccupiedCell        = 0b000000010000
    /// You can't capture en passant because it's been a while since
    /// the opponent's pawn moved
    | HasNoEnPassant        = 0b000000100000
    /// Your sliding or castling move jumps over other pieces,
    /// only knights can do that
    | DoesNotJump           = 0b000001000000
    /// Pawn can't move the way it can capture
    | OnlyCapturesThisWay   = 0b000010000000
    /// Pawn can't capture the way it moves
    | DoesNotCaptureThisWay = 0b000100000000
    /// You can't castle if the square, the King transits through
    /// is under attack right now
    | CastleThroughCheck    = 0b001000000000
    /// The piece doesn't move that way (e.g. bishop cannot move a1-a2)
    | DoesNotMoveThisWay    = 0b010000000000
    /// You can't casle if your king is under check right now
    | CastleFromCheck       = 0b100000000000

/// The part of the chess position that can be compared
/// to determine threefold repetitions
type PositionCore =
    { /// An array of the 64 squares the chess board consists of
      /// Note that index 0 corresponds to a8, and NOT a1!
      /// Indexes read left to right, top to bottom!
      Placement : Piece array
      /// The color of the side that makes the next move
      ActiveColor : Color
      /// Castlings available to the both sides
      /// (one that changes when they move their kings/rooks)
      CastlingAvailability : Castlings
      /// The index is the file the opponent last
      /// made pawn double move -or- ...
      EnPassant : File option }

/// Represents legal move as returned by the legality check
[<StructuredFormatDisplay("{AsString}")>]
type LegalMove =
    { /// The move that was checked for the legality
      Move : Move
      /// The position in which the move was checked
      OriginalPosition : Position
      /// The core of the position gotten as a result of the move
      /// (use legalMove.ToPosition() method to get full position)
      ResultPosition : PositionCore
      /// The piece type that was moved
      Piece : PieceType
      /// The castling, if the move was castling, -or- None
      Castling : Castlings
      /// Annotations (capture, promotion, etc.) to the move
      Observations : MoveAnnotations
      /// Warnings to the move
      Warnings : MoveWarnings }

and
    /// Represents a position in the game
    /// (adding Properties to the position is a bit CPU consuming)
    Position =
    { /// Stuff that was calculated immediately with the legality check
      Core : PositionCore
      /// 50 moves rule counter
      HalfMoveClock : int
      /// Number of full moves (white, then black) counted
      FullMoveNumber : int
      /// Properties of the position like check, mate and stalemate
      Properties : MoveOutcomes
      /// The previous legal move if the position derives from
      /// some other position, -or- ...
      Move : LegalMove option }

/// Represents an illegal move as returned by the legality check
[<StructuredFormatDisplay("{AsString}")>]
[<NoComparison; NoEquality>]
type IllegalMove =
    { /// The move that was checked for the legality
      Move : Move
      /// The position in which the move was checked
      OriginalPosition : Position
      /// The piece type that was moved
      Piece : PieceType
      /// The castling, if the move was castling attempt, -or- None
      Castling : Castlings
      /// Annotations (capture, promotion attempt, etc.) to the move
      Observations : MoveAnnotations
      /// Warnings to the move
      Warnings : MoveWarnings
      /// Non-empty set of the errors to the move
      Errors : MoveErrors }

/// The legality check result
[<NoComparison; NoEquality>]
type MoveInfo =
    /// The legality check succeeded
    | LegalMove of LegalMove
    /// The legality check failed
    | IllegalMove of IllegalMove

// --------------------------------------------------
/// Contains function to work with the Color
module Side =
    /// Converts black to white and white to black
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

/// Contains functions to work with the
/// coordinate as a pair of (file, rank)
[<RequireQualifiedAccess>]
module Coordinate =
    let internal parser =
        let parseFile (p : char) : File = int (p) - int ('a')
        let parseRank c = 8 - (int c - int '0')
        let file = anyOf "abcdefgh" |>> parseFile
        let rank = anyOf "12345678" |>> parseRank
        file .>>. rank

    /// Tries to parse strings like "e3" to the coordinate (file, rank)
    let TryParse(str : string) = run parser str
    /// Parses strings like "e3" to the coordinate (file, rank) -or- throws
    let Parse(str : string) = TryParse str |> Operators.getSuccess
    /// Gets (file, rank) from Idx64
    let FromIdx64 i = (i % 8, i / 8)

    /// Gets the piece on the board by the coordinate (file, rank)
    let At coordinate positionCore =
        let toIdx64 (file, rank) = file + rank * 8
        positionCore.Placement.[toIdx64 coordinate]

    /// Converts the coordinate (file, rank) to string like "e3"
    let ToString(file, rank) = fileToStirng file + rankToString rank

/// Contains functions to work with the
/// square indexes is integets in { 0.. 63 }
[<RequireQualifiedAccess>]
module Idx64 =
    /// Gets the color of the square on the board by index
    let GetColor(idx64 : int) =
        let file, rank = Coordinate.FromIdx64 idx64
        if (file % 2) = (rank % 2) then Color.White
        else Color.Black

    /// Gets the { 0..63 } index from the coordinate (file, rank)
    let FromCoordinate(file, rank) = file + rank * 8
    let internal parser = Coordinate.parser |>> FromCoordinate
    let internal fromX88 i = i % 16 + (i / 16) * 8
    /// Gets the { 0..7 } rank from the { 0..63 } index
    let Rank(idx64 : int) = idx64 / 8
    /// Gets the { 0..7 } file from the { 0..63 } index
    let File(idx64 : int) = idx64 % 8
    /// Tries to parse strings like "e3" into the { 0..63 } index
    let TryParse(str : string) = run parser str
    /// Parses strings like "e3" into the { 0..63 } index -or- throws
    let Parse(str : string) = TryParse str |> Operators.getSuccess
    /// Converts the coordinate (file, rank) to string like "e3"
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

    /// Tries to parse strings like "e2-e4"
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

    /// Parses strings like "e2-e4" -or- throws
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
    /// Gets the nice string representation of the position
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
