module IsAttackedBy

open Definitions

let inline getScanners side at88 square =
    let rec iterate square increment pieceType = 
        let next = square + increment
        if next &&& 0x88 <> 0 then -1
        else if at88 next = Some((side, pieceType)) then next
        else if at88 next <> None then -1
        else iterate next increment pieceType
    
    let check square pieceType = 
        if square &&& 0x88 <> 0 then -1
        else if (at88 square = Some((side, pieceType))) then square
        else -1
    
    let one pieceType = 
        Seq.map (fun increment () -> check (square + increment) pieceType)
    let scan pieceType = 
        Seq.map (fun increment () -> iterate square increment pieceType)
    (one, scan)

let IsAttackedBy side at88 square = 
    let one, scan = getScanners side at88 square

    [ one Pawn (if side = Black then [ -15; -17 ] else [ +15; +17 ])
      one Knight [ -33; -31; -18; -14; +33; +31; +18; +14 ]
      scan Queen [ +15; +17; -15; -17; +16; +01; -16; -01 ]
      scan Rook [ +16; +01; -16; -01 ]
      scan Bishop [ +15; +17; -15; -17 ]
      one King [ +15; +17; -15; -17; +16; +01; -16; -01 ] ]
    |> Seq.concat
    |> Seq.exists (fun f -> f() <> -1)

let FindKing color at64 = 
    let index = 
        seq { 0..63 } 
        |> Seq.tryFindIndex (fun i -> (at64 (i % 8, i / 8)) = (Some(color, King)))
    match index with
    | Some(i) -> Some((i % 8, i / 8) |> toX88)
    | None -> None

let IsInCheck side at64 =
    match FindKing side at64 with
    | Some(position) -> position |> IsAttackedBy (Color.opposite side) (fromX88 >> at64)
    | None -> false
