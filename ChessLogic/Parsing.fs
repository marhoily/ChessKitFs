module ChessKit.ChessLogic.Parsing


let parsePieceLetter = function 
    | 'P' -> (White, Pawn)
    | 'N' -> (White, Knight)
    | 'B' -> (White, Bishop)
    | 'R' -> (White, Rook)
    | 'Q' -> (White, Queen)
    | 'K' -> (White, King)
    | 'p' -> (Black, Pawn)
    | 'n' -> (Black, Knight)
    | 'b' -> (Black, Bishop)
    | 'r' -> (Black, Rook)
    | 'q' -> (Black, Queen)
    | 'k' -> (Black, King)
    | _ -> failwith ("unknown piece letter")
    
let parsePromotionHint = function 
    | 'N' -> Knight
    | 'B' -> Bishop
    | 'R' -> Rook
    | 'Q' -> Queen
    | _ -> failwith ("unknown promotion hint")