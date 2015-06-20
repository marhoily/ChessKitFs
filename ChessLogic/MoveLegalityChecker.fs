﻿module ChessKit.ChessLogic.MoveLegalityChecker

open Operators
open ScanningExtensions

type MoveInfo = 
    | LegalMove of LegalMove
    | IllegalMove of IllegalMove

let ValidateMove move position = 
    let errors = ref []
    let observations = ref []
    let warnings = ref []
    let castling = ref None
    let newPosition = ref None
    let err e = errors := e :: !errors
    let warn w = warnings := w :: !warnings
    let info i = observations := i :: !observations
    let enPassant() = observations := Capture :: EnPassant :: !observations
    let castle x = castling := Some(x)
    
    let hasNoEnPassant() = 
        err HasNoEnPassant
        enPassant()
    
    //   ___________
    //__/ Shortcats \_____________________________________________________
    let moveFrom, moveTo, promoteTo = 
        (move.Start, move.End, move.PromoteTo ?|? Queen)
    let positionCore = position.Core
    let at64 i64 = positionCore |> X88.PieceAt i64
    let at i = positionCore |> X88.PieceAt(i % 16, i / 16)
    let color = positionCore.ActiveColor
    match at64 moveTo with
    | Some(clr, _) when clr = color -> err ToOccupiedCell
    | Some(_) -> info Capture
    | None -> ()
    let pieceType : PieceType option = 
        match at64 moveFrom with
        | Some(pieceColor, fPt) -> 
            if color <> pieceColor then err WrongSideToMove
            Some(fPt)
        | None -> 
            err EmptyCell
            None
    
    //   _______________________
    //__/ Validateion functions \_________________________________________
    let validatePawnMove fromSquare toSquare = 
        let validateDoublePush v c = 
            if fromSquare / 16 <> c then err DoesNotMoveThisWay
            else if at toSquare <> None then err DoesNotCaptureThisWay
            else if at (fromSquare + v) <> None then err DoesNotJump
            else info DoublePush
        
        let validatePush c = 
            if at toSquare <> None then err DoesNotCaptureThisWay
            else 
                if fromSquare / 16 = c then info Promotion
        
        let validateCapture c2 looksEnPassanty = 
            if at toSquare = None then 
                if looksEnPassanty() then 
                    if positionCore.EnPassant = Some(toSquare % 16) then 
                        enPassant()
                    else hasNoEnPassant()
                else err OnlyCapturesThisWay
            else 
                if fromSquare / 16 = c2 then info Promotion
        
        let looksEnPassanty c1 c2 c3 clr () = 
            fromSquare / 16 = c1 && at (fromSquare + c2) = None 
            && at (fromSquare + c3) = (Some(clr, Pawn))
        match (color, (toSquare - fromSquare)) with
        | (White, -32) -> validateDoublePush -16 6
        | (Black, +32) -> validateDoublePush +16 1
        | (White, -16) -> validatePush 1
        | (Black, +16) -> validatePush 6
        | (White, -15) -> validateCapture 1 (looksEnPassanty 3 -31 +1 Black)
        | (White, -17) -> validateCapture 1 (looksEnPassanty 3 -33 -1 Black)
        | (Black, +17) -> validateCapture 6 (looksEnPassanty 4 +33 +1 White)
        | (Black, +15) -> validateCapture 6 (looksEnPassanty 4 +31 -1 White)
        | _ -> err DoesNotMoveThisWay
    
    let validateKnightMove f t = 
        match (t - f) with
        | 33 | 31 | -33 | -31 | 18 | 14 | -18 | -14 -> ()
        | _ -> err DoesNotMoveThisWay
    
    let validateKingMove fromSquare toSquare = 
        let avail opt = positionCore.CastlingAvailability |> List.contains opt
        
        let long B C D E attacked castlingOpt = 
            if at D <> None || at B <> None then err DoesNotJump
            else if at C <> None then err DoesNotCaptureThisWay
            else if not (avail castlingOpt) then err HasNoCastling
            else if attacked E then err CastleFromCheck
            else 
                if attacked D then err CastleThroughCheck
            castle castlingOpt
        
        let short E F G attacked castlingOpt = 
            if at F <> None then err DoesNotJump
            else if at G <> None then err DoesNotCaptureThisWay
            else if not (avail castlingOpt) then err HasNoCastling
            else if attacked E then err CastleFromCheck
            else 
                if attacked F then err CastleThroughCheck
            castle castlingOpt
        
        let w = position.Core |> IsAttackedBy Black
        let b = position.Core |> IsAttackedBy White
        match (toSquare - fromSquare) with
        | 1 | 15 | 16 | 17 | -1 | -15 | -16 | -17 -> ()
        | -2 | +2 -> 
            match (fromSquare, toSquare) with
            | (E1, C1) -> long B1 C1 D1 E1 w WQ
            | (E8, C8) -> long B8 C8 D8 E8 b BQ
            | (E1, G1) -> short E1 F1 G1 w WK
            | (E8, G8) -> short E8 F8 G8 b BK
            | _ -> err DoesNotMoveThisWay
        | _ -> err DoesNotMoveThisWay
    
    let validateSlidingMove offsets f t = 
        let rec iterate start stop increment = 
            let next = start + increment
            if next &&& 0x88 <> 0 then err DoesNotMoveThisWay
            else if next = stop then ()
            else if at next <> None then err DoesNotJump
            else iterate next stop increment
        
        let isMultipleOf n m = n % m = 0 && n / m < 8 && n / m >= 0
        match offsets |> Seq.tryFind (t - f |> isMultipleOf) with
        | Some(m) -> iterate f t m
        | None -> err DoesNotMoveThisWay
    
    let validateBishopMove = validateSlidingMove [ 15; -15; 17; -17 ]
    let validateRookMove = validateSlidingMove [ 16; -16; 01; -01 ]
    let validateQueenMove = 
        validateSlidingMove [ 16; -16; 01; -01; 15; -15; 17; -17 ]
    
    let validateByPieceType() = 
        match pieceType.Value with
        | Pawn -> validatePawnMove
        | Knight -> validateKnightMove
        | King -> validateKingMove
        | Bishop -> validateBishopMove
        | Rook -> validateRookMove
        | Queen -> validateQueenMove
    
    //   _______
    //__/ Steps \_________________________________________________________
    let validate() = validateByPieceType () (X88.fromTuple moveFrom) (X88.fromTuple moveTo)
    
    let setupResultPosition() = 
        let newPlacement = Array.copy positionCore.Placement
        // Remove the pawn captured en-passant
        if !observations |> List.contains EnPassant then 
            let increment = 
                if color = White then +8
                else -8
            newPlacement.[(moveTo |> X88.to64) + increment] <- None
        // Remove the piece from the old square and put it to the new square
        let effectivePiece : PieceType = 
            if !observations |> List.contains Promotion then promoteTo
            else pieceType.Value
        newPlacement.[moveTo |> X88.to64] <- Some((color, effectivePiece))
        newPlacement.[moveFrom |> X88.to64] <- None
        // Move the rook if it was a castling
        let moveCastlingRook f t = 
            let x88toIndex = Coordinate.fromX88 >> X88.to64
            let rook = newPlacement.[Coordinate.fromX88 f |> X88.to64]
            newPlacement.[f |> x88toIndex] <- None
            newPlacement.[t |> x88toIndex] <- rook
        match !castling with
        | Some(WK) -> moveCastlingRook H1 F1
        | Some(WQ) -> moveCastlingRook A1 D1
        | Some(BK) -> moveCastlingRook H8 F8
        | Some(BQ) -> moveCastlingRook A8 D8
        | None -> ()
        // Figure out new castling availability
        let optionsInvalidatedBy p = 
            match p |> X88.fromTuple with
            | A1 -> [ WQ ]
            | E1 -> [ WQ; WK ]
            | H1 -> [ WK ]
            | A8 -> [ BQ ]
            | E8 -> [ BQ; BK ]
            | H8 -> [ BK ]
            | _ -> []
        
        let newCastlingAvailability = 
            positionCore.CastlingAvailability
            |> List.except (optionsInvalidatedBy moveFrom)
            |> List.except (optionsInvalidatedBy moveTo)
        
        // Figure out new en-passant option
        let newEnPassant = 
            if !observations |> List.contains DoublePush then Some(fst moveFrom)
            else None
        
        // Construct new position
        let updatedPosition = 
            { positionCore with Placement = newPlacement
                                ActiveColor = color.Invert
                                EnPassant = newEnPassant
                                CastlingAvailability = newCastlingAvailability }
        newPosition := Some(updatedPosition)
    
    let setMoveToCheck() = 
        if IsInCheck color (!newPosition).Value then 
            err MoveToCheck
            newPosition := None
    
    let setRequiresPromotion() = 
        let requiresPromotion = !observations |> List.contains Promotion
        if move.PromoteTo = None then 
            if requiresPromotion then warn MissingPromotionHint
        else 
            if not requiresPromotion then warn PromotionHintIsNotNeeded
    
    //   __________
    //__/ Do steps \______________________________________________________    
    List.iter (fun f -> 
        if (!errors).IsEmpty then f()) 
        [ validate; setupResultPosition; setMoveToCheck; setRequiresPromotion ]
    if (!errors).IsEmpty then 
        LegalMove { Move = move
                    OriginalPosition = position
                    ResultPosition = (!newPosition).Value
                    Piece = pieceType.Value
                    Castling = !castling
                    Observations = !observations
                    Warnings = !warnings } 
    else 
        IllegalMove { Move = move
                      OriginalPosition = position
                      Piece = pieceType
                      Castling = !castling
                      Observations = !observations
                      Warnings = !warnings
                      Errors = !errors } 

let ValidateLegalMove move pos = 
    match ValidateMove move pos with
    | LegalMove m -> m
    | IllegalMove(_) -> failwith "move is illegal"
