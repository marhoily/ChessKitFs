module FenPrinterTests

open Xunit
open ChessKit.ChessLogic
open FsUnit.Xunit

let internal EmptyPosition = 
    { Core = 
          { Placement = [||]
            ActiveColor = White
            CastlingAvailability = [ WK; WQ; BK; BQ ]
            EnPassant = None }
      HalfMoveClock = 0
      FullMoveNumber = 1
      Observations = []
      Move = None }

let toFenCore c = 
    Fen.Print { EmptyPosition with Core = c } 

let ec = EmptyPosition.Core;

[<Fact>]
let ``ToFen works``() = 
    toFenCore { ec with Placement = [| Some(White, Pawn) |] } 
    |> should equal "P w KQkq - 0 1"

[<Fact>]
let ``ToFen works with one empty square``() = 
    toFenCore { ec with Placement = 
                                   [| None
                                      Some(White, Pawn) |] }
    |> should equal "1P w KQkq - 0 1"

[<Fact>]
let ``ToFen works with two empty squares``() = 
    toFenCore { ec with Placement = 
                                   [| None
                                      None
                                      Some(White, Pawn) |] }
    |> should equal "2P w KQkq - 0 1"

[<Fact>]
let ``ToFen works with empty squares in the middle``() = 
    toFenCore { ec with Placement = 
                                   [| Some(White, Pawn)
                                      None
                                      None
                                      Some(Black, Bishop) |] }
    |> should equal "P2b w KQkq - 0 1"

[<Fact>]
let ``ToFen works with 2 ranks``() = 
    toFenCore { ec with Placement = 
                                   [| Some(White, Pawn)
                                      None
                                      None
                                      None
                                      None
                                      None
                                      None
                                      None
                                      Some(Black, Bishop) |] }
    |> should equal "P7/b w KQkq - 0 1"

[<Fact>]
let ``ToFen works with piece on the H file``() = 
    toFenCore { ec with Placement = 
                                   [| Some(White, Pawn)
                                      None
                                      None
                                      None
                                      None
                                      None
                                      None
                                      Some(Black, Bishop) |] }
    |> should equal "P6b w KQkq - 0 1"

[<Fact>]
let ``ToFen works with empty ranks``() = 
    toFenCore { ec with Placement = 
                                   [| None
                                      None
                                      None
                                      None
                                      None
                                      None
                                      None
                                      None
                                      Some(Black, Bishop) |] }
    |> should equal "8/b w KQkq - 0 1"

[<Fact>]
let ``ToFen works with empty rank after non-empty rank``() = 
    toFenCore { ec with Placement = 
                                   [| Some(White, Pawn)
                                      None
                                      None
                                      None
                                      None
                                      None
                                      None
                                      None
                                      None
                                      None
                                      None
                                      None
                                      None
                                      None
                                      None
                                      None
                                      Some(Black, Bishop) |] }
    |> should equal "P7/8/b w KQkq - 0 1"

[<Fact>]
let ``ToFen works with round number of squares``() = 
    toFenCore { ec with Placement = 
                                   [| Some(White, Pawn)
                                      None
                                      None
                                      None
                                      None
                                      None
                                      None
                                      None |] }
    |> should equal "P7 w KQkq - 0 1"

[<Fact>]
let ``ToFen prints en-passant for white correctly``() = 
    toFenCore { ec with EnPassant = Some(4) } 
    |> should equal " w KQkq e6 0 1"

[<Fact>]
let ``ToFen prints en-passant for black correctly``() = 
    toFenCore { ec with EnPassant = Some(4)
                        ActiveColor = Black }
    |> should equal " b KQkq e3 0 1"

[<Fact>]
let ``ToFen prints castling availability correctly when some unavailable``() = 
    toFenCore { ec with CastlingAvailability = [] } 
    |> should equal " w - - 0 1"
