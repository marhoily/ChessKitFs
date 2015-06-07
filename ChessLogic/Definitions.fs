module Definitions

open System.Text

type File = int

let fileToStirng (f : File) = char(int 'a' + f) |> string
let LetterToFileNoCheck(p : char) : File = int (p) - int ('a')

type Rank = int

let rankToString (rank : Rank) = (8 - rank).ToString()

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

[<Literal>]
let internal A1 = 112

[<Literal>]
let internal B1 = 113

[<Literal>]
let internal C1 = 114

[<Literal>]
let internal D1 = 115

[<Literal>]
let internal E1 = 116

[<Literal>]
let internal F1 = 117

[<Literal>]
let internal G1 = 118

[<Literal>]
let internal H1 = 119

[<Literal>]
let internal A8 = 0

[<Literal>]
let internal B8 = 1

[<Literal>]
let internal C8 = 2

[<Literal>]
let internal D8 = 3

[<Literal>]
let internal E8 = 4

[<Literal>]
let internal F8 = 5

[<Literal>]
let internal G8 = 6

[<Literal>]
let internal H8 = 7