module ChessKit.ChessLogic.IsAttackedBy

open X88

let inline getScanners side at88 square = 
    let rec slide square pieceType increment () = 
        let next = square + increment
        if next &&& 0x88 <> 0 then -1
        else if at88 next = Some(side, pieceType) then next
        else if at88 next <> None then -1
        else slide next pieceType increment ()
    
    let jump square pieceType increment () = 
        let next = square + increment
        if next &&& 0x88 <> 0 then -1
        else if at88 next = Some(side, pieceType) then next
        else -1
    
    let scan fn pieceType = Seq.map (fn square pieceType)
    (scan jump, scan slide)

let IsAttackedBy side (position: PositionCore) c88 = 
    let jump, slide = getScanners side position.atX88 c88
    [ jump Pawn (if side = Black then [ -15; -17 ]
                else [ +15; +17 ])
      jump Knight [ -33; -31; -18; -14; +33; +31; +18; +14 ]
      slide Queen [ +15; +17; -15; -17; +16; +01; -16; -01 ]
      slide Rook [ +16; +01; -16; -01 ]
      slide Bishop [ +15; +17; -15; -17 ]
      jump King [ +15; +17; -15; -17; +16; +01; -16; -01 ] ]
    |> Seq.concat
    |> Seq.exists (fun f -> f() <> -1)

let FindKing color (position: PositionCore) = 
    let index = 
        seq { 0..63 } 
        |> Seq.map position.at64
        |> Seq.tryFindIndex 
               (fun i -> i = (Some(color, King)))
    match index with
    | Some(i) -> Some((i % 8, i / 8) |> toX88)
    | None -> None

let IsInCheck (side:Color) (position: PositionCore) = 
    match FindKing side position with
    | Some c -> IsAttackedBy side.Invert position c
    | None -> false
