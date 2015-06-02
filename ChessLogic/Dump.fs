module Dump

open System.Text
open Definitions

let Print(board : Position) = 
    let sb = new StringBuilder(17 * 36)
    sb.AppendLine(" ╔═══╤═══╤═══╤═══╤═══╤═══╤═══╤═══╗") |> ignore
    sb.AppendLine("8║ 1 │ 2 │ 3 │ 4 │ 5 │ 6 │ 7 │ 8 ║") |> ignore
    sb.AppendLine(" ╟───┼───┼───┼───┼───┼───┼───┼───╢") |> ignore
    sb.AppendLine("7║ 1 │ 2 │ 3 │ 4 │ 5 │ 6 │ 7 │ 8 ║") |> ignore
    sb.AppendLine(" ╟───┼───┼───┼───┼───┼───┼───┼───╢") |> ignore
    sb.AppendLine("6║ 1 │ 2 │ 3 │ 4 │ 5 │ 6 │ 7 │ 8 ║") |> ignore
    sb.AppendLine(" ╟───┼───┼───┼───┼───┼───┼───┼───╢") |> ignore
    sb.AppendLine("5║ 1 │ 2 │ 3 │ 4 │ 5 │ 6 │ 7 │ 8 ║") |> ignore
    sb.AppendLine(" ╟───┼───┼───┼───┼───┼───┼───┼───╢") |> ignore
    sb.AppendLine("4║ 1 │ 2 │ 3 │ 4 │ 5 │ 6 │ 7 │ 8 ║") |> ignore
    sb.AppendLine(" ╟───┼───┼───┼───┼───┼───┼───┼───╢") |> ignore
    sb.AppendLine("3║ 1 │ 2 │ 3 │ 4 │ 5 │ 6 │ 7 │ 8 ║") |> ignore
    sb.AppendLine(" ╟───┼───┼───┼───┼───┼───┼───┼───╢") |> ignore
    sb.AppendLine("2║ 1 │ 2 │ 3 │ 4 │ 5 │ 6 │ 7 │ 8 ║") |> ignore
    sb.AppendLine(" ╟───┼───┼───┼───┼───┼───┼───┼───╢") |> ignore
    sb.AppendLine("1║ 1 │ 2 │ 3 │ 4 │ 5 │ 6 │ 7 │ 8 ║") |> ignore
    sb.AppendLine(" ╚═══╧═══╧═══╧═══╧═══╧═══╧═══╧═══╝") |> ignore
    sb.AppendLine("   A   B   C   D   E   F   G   H  ") |> ignore
    for position = 0 to 63 do
        let piece = board.Placement.[position]
        let file = position % 8
        let rank = position / 8
        let index = (rank * 2 + 1) * 36 + file * 4 + 3
        sb.[index] <- (match piece with
                       | None -> ' '
                       | Some(p) -> PieceToString p)
    string sb
