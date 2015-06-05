module Definitions

open System.Text

type File = int

let printFile (f : File) = char(int 'a' + f) |> string
let LetterToFileNoCheck(p : char) : File = int (p) - int ('a')

type Rank = int

let rankToString (rank : Rank) = (8 - rank).ToString()

type Coordinate = File * Rank

// https://chessprogramming.wikispaces.com/0x88
let toX88 = function 
    | (x, y) -> x + y * 16
let fromX88 i = (i % 16, i / 16)
let CoordinateToString = function 
    | (file, rank) -> printFile file + rankToString rank

type Color = 
    | Black
    | White
    
    static member toString = 
        function 
        | White -> 'w'
        | Black -> 'b'
    
    static member opposite = 
        function 
        | White -> Black
        | Black -> White
    
    static member oppositeOf = Color.opposite

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

type Position = 
    { Placement : Piece option array
      ActiveColor : Color
      CastlingAvailability : CastlingHint list
      EnPassant : File option
      HalfMoveClock : int
      FullMoveNumber : int
      Observations : PositionObservation list }

let EmptyPosition = 
    { Placement = [||]
      ActiveColor = White
      CastlingAvailability = [ WK; WQ; BK; BQ ]
      EnPassant = None
      HalfMoveClock = 0
      FullMoveNumber = 1
      Observations = [] }

let BoardToString b = 
    let sb = new StringBuilder()
    let mutable counter = 0
    for s in b.Placement do
        counter <- counter + 1
        match s with
        | Some(p) -> sb.Append(PieceToString p) |> ignore
        | None -> sb.Append('.') |> ignore
        if counter % 8 = 0 then sb.AppendLine() |> ignore
    string sb
