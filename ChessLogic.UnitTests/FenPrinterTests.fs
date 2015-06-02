module FenPrinterTests

open Xunit
open Definitions
open FenPrinter
open FsUnit.Xunit

[<Fact>]
let ``ToFen works``() = 
    ToFen { EmptyPosition with Placement = [| Some(White, Pawn) |] } 
    |> should equal "P w KQkq - 0 1"

[<Fact>]
let ``ToFen works with one empty square``() = 
    ToFen { EmptyPosition with Placement = 
                                   [| None
                                      Some(White, Pawn) |] }
    |> should equal "1P w KQkq - 0 1"

[<Fact>]
let ``ToFen works with two empty squares``() = 
    ToFen { EmptyPosition with Placement = 
                                   [| None
                                      None
                                      Some(White, Pawn) |] }
    |> should equal "2P w KQkq - 0 1"

[<Fact>]
let ``ToFen works with empty squares in the middle``() = 
    ToFen { EmptyPosition with Placement = 
                                   [| Some(White, Pawn)
                                      None
                                      None
                                      Some(Black, Bishop) |] }
    |> should equal "P2b w KQkq - 0 1"

[<Fact>]
let ``ToFen works with 2 ranks``() = 
    ToFen { EmptyPosition with Placement = 
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
    ToFen { EmptyPosition with Placement = 
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
    ToFen { EmptyPosition with Placement = 
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
    ToFen { EmptyPosition with Placement = 
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
    ToFen { EmptyPosition with Placement = 
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
    ToFen { EmptyPosition with EnPassant = Some(4) } 
    |> should equal " w KQkq e6 0 1"

[<Fact>]
let ``ToFen prints en-passant for black correctly``() = 
    ToFen { EmptyPosition with EnPassant = Some(4)
                               ActiveColor = Black }
    |> should equal " b KQkq e3 0 1"

[<Fact>]
let ``ToFen prints castling availability correctly when some unavailable``() = 
    ToFen { EmptyPosition with CastlingAvailability = [] } 
    |> should equal " w - - 0 1"
