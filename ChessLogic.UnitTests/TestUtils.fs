module TestUtils

open FsUnit.Xunit
open ChessKit.ChessLogic

let ParseToStringShouldMatch toString parse input = 
    toString (parse input) |> should equal input
let ErrorMessageShouldMatch parse (input : string) (msg : string) = 
    (Operators.getError (parse input)).Replace("\r\n", "\n") |> should equal (msg.Replace("\r\n", "\n"))

let EmptyPosition = 
    { Core = 
          { Placement = [||]
            ActiveColor = White
            CastlingAvailability = Castlings.All
            EnPassant = None }
      HalfMoveClock = 0
      FullMoveNumber = 1
      Properties = Properties.None
      Move = None }
