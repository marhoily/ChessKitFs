[<RequireQualifiedAccess>]
module ChessKit.ChessLogic.Fen

open Text
open System
open System.Text
open Operators

let Print p = 
    let color = 
        match p.Core.ActiveColor with
        | Color.White -> "w"
        | Color.Black -> "b"
        | _ -> failwith "unexpected"
    
    let castling = 
        let specialCase predicate value source = 
            if predicate source then value else source

        [ (Castlings.WK, 'K');
          (Castlings.WQ, 'Q');
          (Castlings.BK, 'k');
          (Castlings.BQ, 'q') ]
        |> Seq.filter (fst >> test p.Core.CastlingAvailability) 
        |> Seq.map snd |> Seq.toArray
        // This can be just "String" in F# 4.0
        |> (fun arr -> (String(arr)))
        |> specialCase ((=) "") "-"
    
    let enPassant = 
        match p.Core.EnPassant with
        | Some(file) -> 
            (fileToStirng file) + (if p.Core.ActiveColor = Color.Black then "3"
                                   else "6")
        | None -> "-"
    
    let sb = new StringBuilder()
    let appendc (c : char) = sb.Append(c) |> ignore
    let appends (s : string) = sb.Append(s) |> ignore
    let appendi (i : int) = sb.Append(i) |> ignore
    let mutable skip = 0
    let mutable file = 0
    for square in p.Core.Placement do
        if file = 8 then 
            appendc '/'
            file <- 0
        file <- file + 1
        match square with
        | Piece.EmptyCell -> skip <- skip + 1
        | p -> 
            if skip > 0 then 
                appendi skip
                skip <- 0
            appendc (pieceToChar p)
        if file = 8 && skip > 0 then 
            appendi skip
            skip <- 0
    appendc ' '
    appends color
    appendc ' '
    appends castling
    appendc ' '
    appends enPassant
    appendc ' '
    appendi p.HalfMoveClock
    appendc ' '
    appendi p.FullMoveNumber
    string sb

open FParsec
open Microsoft.FSharp.Core.Option

type private Code = 
    | Piece of Piece
    | Gap of int

let TryParse str = 
    let parsePieceLetter = 
        function 
        | 'P' -> Piece.WhitePawn
        | 'N' -> Piece.WhiteKnight
        | 'B' -> Piece.WhiteBishop
        | 'R' -> Piece.WhiteRook
        | 'Q' -> Piece.WhiteQueen
        | 'K' -> Piece.WhiteKing
        | 'p' -> Piece.BlackPawn
        | 'n' -> Piece.BlackKnight
        | 'b' -> Piece.BlackBishop
        | 'r' -> Piece.BlackRook
        | 'q' -> Piece.BlackQueen
        | 'k' -> Piece.BlackKing
        | _ -> failwith ("unknown piece letter")
    
    let parseGap c = Gap(int c - int '0')
    let parsePiece c = Piece(parsePieceLetter c)
    let fold = List.fold (|||) Castlings.None

    let castlingHintParse = 
        function 
        | 'Q' -> Castlings.WQ
        | 'K' -> Castlings.WK
        | 'q' -> Castlings.BQ
        | 'k' -> Castlings.BK
        | _ -> failwith "unknown castling symbol"
    
    let parsePlacement ranks = 
        [| for rank in ranks do
               for square in rank do
                   match square with
                   | Piece(p) -> yield p
                   | Gap(n) -> for _ in 1..n -> Piece.EmptyCell |]
    
    let createCore plcmnt clr ca enp = 
        { Placement = parsePlacement (plcmnt)
          ActiveColor = clr
          CastlingAvailability = ca
          EnPassant = map fst enp }
    
    let createPosition core halfMoveClock fullMoveNumber = 
        { Core = core
          HalfMoveClock = halfMoveClock
          FullMoveNumber = fullMoveNumber
          Properties = MoveOutcomes.None
          Move = None }
    
    let piece = anyOf "pnbrqkPNBRQK" |>> parsePiece <?> "piece symbol"
    let gap = anyOf "12345678" |>> parseGap <?> "number 1..8"
    let rank = many1 (piece <|> gap)
    let ws = pchar ' '
    let ranks = sepBy1 rank (pchar '/') .>> ws
    let black = pchar 'b' >>% Color.Black
    let white = pchar 'w' >>% Color.White
    let color = black <|> white .>> ws
    let castlingHint = anyOf "KQkq" |>> castlingHintParse
    let castlings = many1 castlingHint |>> fold
    let noCastling = pchar '-' >>% Castlings.None
    let ca = noCastling <|> castlings .>> ws
    let file = anyOf "abcdefgh" |>> parseFile
    let enColor = (pchar '3' >>% Color.Black) <|> (pchar '6' >>% Color.White)
    let en = (file .>>. enColor) |>> Some
    let enPassant = ((pchar '-' >>% None) <|> en) .>> ws
    let core = pipe4 ranks color ca enPassant createCore
    let n = pint32
    let fenParser = pipe3 core (n .>> ws) n createPosition
    run fenParser str

let Parse str = TryParse str |> Operators.getSuccess
let ParseCore str = (Parse str).Core

