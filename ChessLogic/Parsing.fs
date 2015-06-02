module Parsing

open Definitions
open FParsec.CharParsers

type Result<'a, 'b> = 
    | Ok of 'a
    | Error of 'b

let unwrap = function 
    | Ok(p) -> p
    | Error(e) -> failwith (e.ToString())

let peel = function 
    | Ok(p) -> p
    | Error(p) -> p

let wrap = function 
    | Success(p, _, _) -> Ok(p)
    | Failure(e, _, _) -> Error(e)

let getErrorMessage = function 
    | Ok(_) -> failwith "we expected error, but the result was actually a success"
    | Error(e) -> e

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
    
let parsePieceType = function 
    | 'P' | 'p' -> Pawn
    | 'N' | 'n' -> Knight
    | 'B' | 'b' -> Bishop
    | 'R' | 'r' -> Rook
    | 'Q' | 'q' -> Queen
    | 'K' | 'k' -> King
    | _ -> failwith ("unknown piece letter")
    
let parseColorLetter = function 
    | 'w' -> White
    | 'b' -> Black
    | _ -> failwith ("unknown color letter")
    
