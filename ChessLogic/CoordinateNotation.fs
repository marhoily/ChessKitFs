module ChessKit.ChessLogic.CoordinateNotation

open Text
open FParsec

let private coordinate = 
    let parseFile (p : char) : File = int (p) - int ('a')
    let parseRank c = 8 - (int c - int '0')
    let file = anyOf "abcdefgh" |>> parseFile
    let rank = anyOf "12345678" |>> parseRank
    file .>>. rank

let TryParseCoordinateNotation str = 
    let parsePromotionHint = 
        function 
        | 'N' -> Knight
        | 'B' -> Bishop
        | 'R' -> Rook
        | 'Q' -> Queen
        | _ -> failwith ("unknown promotion hint")
    
    let f = coordinate .>> (pchar '-' <|> pchar 'x')
    let hint = anyOf "NBRQK" |>> parsePromotionHint
    let p = opt ((pchar '=') >>. hint)
    let notation = pipe3 f coordinate p (Move.Create)
    run notation str

let TryParseCoordinate str = run coordinate str
let ParseCoordinate = TryParseCoordinate >> Operators.getSuccess
let ParseCoordinateNotation = TryParseCoordinateNotation >> Operators.getSuccess
