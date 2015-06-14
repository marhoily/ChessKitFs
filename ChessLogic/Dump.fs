module Dump

open System.Text
open Definitions

let Print(board : Position) = 
    let sb = 
        new StringBuilder(" ╔═══╤═══╤═══╤═══╤═══╤═══╤═══╤═══╗\r\n" 
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
    for position = 0 to 63 do
        let piece = board.Core.Placement.[position]
        let file = position % 8
        let rank = position / 8
        let index = (rank * 2 + 1) * 36 + file * 4 + 3
        sb.[index] <- (match piece with
                       | None -> ' '
                       | Some(p) -> PieceToString p)
    string sb
