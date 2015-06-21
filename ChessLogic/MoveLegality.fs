[<RequireQualifiedAccess>]
module internal ChessKit.ChessLogic.MoveLegality

open Operators
open Scanning
open PositionCoreExt

let Validate move position = 
    let errors = ref MoveErrors.None
    let observations = ref MoveAnnotations.None
    let warnings = ref MoveWarnings.None
    let castling = ref Castlings.None
    let newPosition = ref None
    let err e = errors := e ||| !errors
    let warn w = warnings := w ||| !warnings
    let info i = observations := i ||| !observations
    let enPassant() = 
        observations 
        := MoveAnnotations.Capture ||| MoveAnnotations.EnPassant 
           ||| !observations
    
    let hasNoEnPassant() = 
        err MoveErrors.HasNoEnPassant
        enPassant()
    
    //   ___________
    //__/ Shortcats \_____________________________________________________
    let moveFromIdx64, moveToIdx64, promoteTo = 
        (move.FromIdx64, move.ToIdx64, 
         if move.PromoteTo = PieceType.None then PieceType.Queen
         else move.PromoteTo)
    
    let positionCore = position.Core
    let at = positionCore.atX88
    let color = positionCore.ActiveColor
    let pieceTo = positionCore.atIdx64 moveToIdx64
    if pieceTo
       |> getColor
       = color then err MoveErrors.ToOccupiedCell
    else 
        if pieceTo <> Piece.EmptyCell then info MoveAnnotations.Capture
    let pieceFrom = positionCore.atIdx64 moveFromIdx64
    let pieceType = pieceFrom |> getPieceType
    if pieceFrom = Piece.EmptyCell then err MoveErrors.EmptyCell
    else 
        if pieceFrom
           |> getColor
           <> color then err MoveErrors.WrongSideToMove
    //   _______________________
    //__/ Validateion functions \_________________________________________
    let validatePawnMove fromSquare toSquare = 
        let validateDoublePush v c = 
            if fromSquare / 16 <> c then err MoveErrors.DoesNotMoveThisWay
            else if at toSquare <> Piece.EmptyCell then 
                err MoveErrors.DoesNotCaptureThisWay
            else if at (fromSquare + v) <> Piece.EmptyCell then 
                err MoveErrors.DoesNotJump
            else info MoveAnnotations.DoublePush
        
        let validatePush c = 
            if at toSquare <> Piece.EmptyCell then 
                err MoveErrors.DoesNotCaptureThisWay
            else 
                if fromSquare / 16 = c then info MoveAnnotations.Promotion
        
        let validateCapture c2 looksEnPassanty = 
            if at toSquare = Piece.EmptyCell then 
                if looksEnPassanty() then 
                    if positionCore.EnPassant = Some(toSquare % 16) then 
                        enPassant()
                    else hasNoEnPassant()
                else err MoveErrors.OnlyCapturesThisWay
            else 
                if fromSquare / 16 = c2 then info MoveAnnotations.Promotion
        
        let looksEnPassanty c1 c2 c3 clr () = 
            fromSquare / 16 = c1 && at (fromSquare + c2) = Piece.EmptyCell 
            && at (fromSquare + c3) = clr +|+ PieceType.Pawn
        match (color, (toSquare - fromSquare)) with
        | (Color.White, -32) -> validateDoublePush -16 6
        | (Color.Black, +32) -> validateDoublePush +16 1
        | (Color.White, -16) -> validatePush 1
        | (Color.Black, +16) -> validatePush 6
        | (Color.White, -15) -> 
            validateCapture 1 (looksEnPassanty 3 -31 +1 Color.Black)
        | (Color.White, -17) -> 
            validateCapture 1 (looksEnPassanty 3 -33 -1 Color.Black)
        | (Color.Black, +17) -> 
            validateCapture 6 (looksEnPassanty 4 +33 +1 Color.White)
        | (Color.Black, +15) -> 
            validateCapture 6 (looksEnPassanty 4 +31 -1 Color.White)
        | _ -> err MoveErrors.DoesNotMoveThisWay
    
    let validateKnightMove f t = 
        match (t - f) with
        | 33 | 31 | -33 | -31 | 18 | 14 | -18 | -14 -> ()
        | _ -> err MoveErrors.DoesNotMoveThisWay
    
    let validateKingMove fromSquare toSquare = 
        let avail = test positionCore.CastlingAvailability
        
        let long B C D E attacked castlingOpt = 
            if at D <> Piece.EmptyCell || at B <> Piece.EmptyCell then 
                err MoveErrors.DoesNotJump
            else if at C <> Piece.EmptyCell then 
                err MoveErrors.DoesNotCaptureThisWay
            else if not (avail castlingOpt) then err MoveErrors.HasNoCastling
            else if attacked E then err MoveErrors.CastleFromCheck
            else 
                if attacked D then err MoveErrors.CastleThroughCheck
            castling := castlingOpt
        
        let short E F G attacked castlingOpt = 
            if at F <> Piece.EmptyCell then err MoveErrors.DoesNotJump
            else if at G <> Piece.EmptyCell then 
                err MoveErrors.DoesNotCaptureThisWay
            else if not (avail castlingOpt) then err MoveErrors.HasNoCastling
            else if attacked E then err MoveErrors.CastleFromCheck
            else 
                if attacked F then err MoveErrors.CastleThroughCheck
            castling := castlingOpt
        
        let w = position.Core |> IsAttackedBy Color.Black
        let b = position.Core |> IsAttackedBy Color.White
        match (toSquare - fromSquare) with
        | 1 | 15 | 16 | 17 | -1 | -15 | -16 | -17 -> ()
        | -2 | +2 -> 
            match (fromSquare, toSquare) with
            | (E1, C1) -> long B1 C1 D1 E1 w Castlings.WQ
            | (E8, C8) -> long B8 C8 D8 E8 b Castlings.BQ
            | (E1, G1) -> short E1 F1 G1 w Castlings.WK
            | (E8, G8) -> short E8 F8 G8 b Castlings.BK
            | _ -> err MoveErrors.DoesNotMoveThisWay
        | _ -> err MoveErrors.DoesNotMoveThisWay
    
    let validateSlidingMove offsets f t = 
        let rec iterate start stop increment = 
            let next = start + increment
            if next &&& 0x88 <> 0 then err MoveErrors.DoesNotMoveThisWay
            else if next = stop then ()
            else if at next <> Piece.EmptyCell then err MoveErrors.DoesNotJump
            else iterate next stop increment
        
        let isMultipleOf n m = n % m = 0 && n / m < 8 && n / m >= 0
        match offsets |> Seq.tryFind (t - f |> isMultipleOf) with
        | Some(m) -> iterate f t m
        | None -> err MoveErrors.DoesNotMoveThisWay
    
    let validateBishopMove = validateSlidingMove [ 15; -15; 17; -17 ]
    let validateRookMove = validateSlidingMove [ 16; -16; 01; -01 ]
    let validateQueenMove = 
        validateSlidingMove [ 16; -16; 01; -01; 15; -15; 17; -17 ]
    
    let validateByPieceType() = 
        match pieceType with
        | PieceType.Pawn -> validatePawnMove
        | PieceType.Knight -> validateKnightMove
        | PieceType.King -> validateKingMove
        | PieceType.Bishop -> validateBishopMove
        | PieceType.Rook -> validateRookMove
        | PieceType.Queen -> validateQueenMove
        | _ -> failwith "unexpected"
    
    //   _______
    //__/ Steps \_________________________________________________________
    let validate() = 
        validateByPieceType () (X88.fromIdx64 moveFromIdx64) 
            (X88.fromIdx64 moveToIdx64)
    
    let setupResultPosition() = 
        let newPlacement = Array.copy positionCore.Squares
        // Remove the pawn captured en-passant
        if !observations |> test MoveAnnotations.EnPassant then 
            let increment = 
                if color = Color.White then +8
                else -8
            newPlacement.[moveToIdx64 + increment] <- Piece.EmptyCell
        // Remove the piece from the old square and put it to the new square
        let effectivePieceType = 
            if !observations |> test MoveAnnotations.Promotion then promoteTo
            else pieceType
        
        let effectivePiece = color +|+ effectivePieceType
        newPlacement.[moveToIdx64] <- effectivePiece
        newPlacement.[moveFromIdx64] <- Piece.EmptyCell
        // Move the rook if it was a castling
        let moveCastlingRook f t = 
            let rook = newPlacement.[Idx64.fromX88 f]
            newPlacement.[Idx64.fromX88 f] <- Piece.EmptyCell
            newPlacement.[Idx64.fromX88 t] <- rook
        match !castling with
        | Castlings.WK -> moveCastlingRook H1 F1
        | Castlings.WQ -> moveCastlingRook A1 D1
        | Castlings.BK -> moveCastlingRook H8 F8
        | Castlings.BQ -> moveCastlingRook A8 D8
        | _ -> ()
        // Figure out new castling availability
        let optionsInvalidatedBy p = 
            match p |> X88.fromIdx64 with
            | A1 -> Castlings.WQ
            | E1 -> Castlings.W
            | H1 -> Castlings.WK
            | A8 -> Castlings.BQ
            | E8 -> Castlings.B
            | H8 -> Castlings.BK
            | _ -> Castlings.None
        
        let newCastlingAvailability = 
            positionCore.CastlingAvailability 
            &&& ~~~((optionsInvalidatedBy moveFromIdx64) 
                    ||| (optionsInvalidatedBy moveToIdx64))
        
        // Figure out new en-passant option
        let newEnPassant = 
            if !observations |> test MoveAnnotations.DoublePush then 
                Some(Idx64.File moveFromIdx64)
            else None
        
        // Construct new position
        let updatedPosition = 
            { positionCore with Squares = newPlacement
                                ActiveColor = color |> Side.Invert
                                EnPassant = newEnPassant
                                CastlingAvailability = newCastlingAvailability }
        
        newPosition := Some(updatedPosition)
    
    let setMoveToCheck() = 
        if IsInCheck color (!newPosition).Value then 
            err MoveErrors.MoveToCheck
            newPosition := None
    
    let setRequiresPromotion() = 
        let requiresPromotion = !observations |> test MoveAnnotations.Promotion
        if move.PromoteTo = PieceType.None then 
            if requiresPromotion then warn MoveWarnings.MissingPromotionHint
        else 
            if not requiresPromotion then 
                warn MoveWarnings.PromotionHintIsNotNeeded
    
    //   __________
    //__/ Do steps \______________________________________________________    
    List.iter (fun f -> 
        if !errors = MoveErrors.None then f()) 
        [ validate; setupResultPosition; setMoveToCheck; setRequiresPromotion ]
    if !errors = MoveErrors.None then 
        LegalMove { Move = move
                    OriginalPosition = position
                    ResultPosition = (!newPosition).Value
                    Piece = pieceType
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

let ValidateLegal move pos = 
    match Validate move pos with
    | LegalMove m -> m
    | IllegalMove(_) -> failwith "move is illegal"

let ParseLegal move pos = ValidateLegal (Move.Parse move) pos
