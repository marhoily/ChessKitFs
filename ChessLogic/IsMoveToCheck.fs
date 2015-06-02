module IsMoveToCheck

open Definitions
open IsAttackedBy

//let isMoveToCheck position movingPiece kingCoordinate
//				  movingPieceColor fromSquare toSquare capturedPiece =
//
//    let iterate ... : int = 
//        ...
//
//    let findPinningPiece origin target = 
//        match target - origin with
//        | n with n |> isMultipleOf 15 -> iterate 15 [Boshop; Queen]
//        | n with n |> isMultipleOf 17 -> iterate 17 [Boshop; Queen]
//        ...
//        | n with n |> isMultipleOf 01 -> iterate 01 [Rook; Queen]
//        | n w[;ith n |> isMultipleOf 16 -> iterate 16 [Rook; Queen]
//        ...
//
//    let isInbetween a b x
//        match b - a with
//        | n with n |> isMultipleOf 15 -> x - a |> isMultipleOf 15
//        ...
//
//	let moveDiscoversAttackToTheKing =
//	    match findPinningPiece kingCoordinate fromSquare with
//	    | Some(pinningPiece) -> 
//            toSquare |> isInbetween kingCoordinate pinningPiece
//        | None -> false
//
//	if movingPiece |> isKing then
//		toSquare |> IsUnderAttackBy (oppositeOf movingPieceColor)
//	else 
//        // impossible in a natural game but possible in an artificial setup
//		if position.KingAttackers |> Seq.count > 2 then true
//		else if position.KingAttackers |> Seq.count = 1 then
//			let attackerCoordinate = position.KingAttackers |> Seq.single
//			capturedPiece = attackerCoordinate
//			|| toSquare |> isInbetween kingCoordinate attackerCoordinate
//		else moveDiscoversAttackToTheKing
//	
let isMoveToCheck at64 f t fPt color = 

//    let k = at64 |> findKing color
//    let f = toX88 f
//    let t = toX88 t
//    let at88 = fromX88 >> at64 
//        
//    let moveDiscoversCheck = 
//        match f - k with
//        | _ -> false
//
//    match fPt with
//    | King -> k |> IsAttackedBy color at88
//    | _ -> moveDiscoversCheck

    false