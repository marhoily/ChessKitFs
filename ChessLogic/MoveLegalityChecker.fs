﻿module MoveLegalityChecker

open CoordinateNotation
open Definitions
open IsAttackedBy
open MyList

type Observation = 
    | Capture
    | EnPassant
    | Promotion
    | DoublePush

type Warning = 
    | MissingPromotionHint
    | PromotionHintIsNotNeeded

type Error = 
    | MoveToCheck
    | EmptyCell
    | WrongSideToMove
    | HasNoCastling
    | ToOccupiedCell
    | HasNoEnPassant
    | DoesNotJump
    | OnlyCapturesThisWay
    | DoesNotCaptureThisWay
    | CastleThroughCheck
    | DoesNotMoveThisWay
    | CastleFromCheck

type LegalMove = 
    { Start : Coordinate
      End : Coordinate
      PromoteTo : PieceType
      OriginalPosition : Position
      ResultPosition : Position
      Piece : PieceType
      Castling : CastlingHint option
      Observations : Observation list
      Warnings : Warning list }

type IllegalMove = 
    { Move : Move
      OriginalPosition : Position
      Piece : PieceType option
      Castling : CastlingHint option
      Observations : Observation list
      Warnings : Warning list
      Errors : Error list }

type MoveInfo = 
    | LegalMove of LegalMove
    | IllegalMove of IllegalMove

let ValidateMove move position = 
    let mutable errors = []
    let mutable observations = []
    let mutable warnings = []
    let mutable castling = None
    let mutable resultPosition = None
    let err e = errors <- e :: errors
    let info i = observations <- i :: observations
    let enPassant() = observations <- Capture :: EnPassant :: observations
    let castle x = castling <- Some(x)
    
    let hasNoEnPassant() = 
        err HasNoEnPassant
        enPassant()
    
    //   ___________
    //__/ Shortcats \_____________________________________________________
    let moveFrom, moveTo, promoteTo = 
        match move with
        | UsualMove(f, t) -> (f, t, Queen)
        | PromotionMove({ Vector = (f, t); PromoteTo = p }) -> (f, t, p)
    
    let at64 i64 = position |> PieceAt i64
    let at i = position |> PieceAt(i % 16, i / 16)
    let color = position.ActiveColor
    match at64 moveTo with
    | Some(clr, _) when clr = color -> err ToOccupiedCell
    | Some(_) -> info Capture
    | None -> ()
    let pieceType = 
        match at64 moveFrom with
        | Some(pieceColor, fPt) -> 
            if color <> pieceColor then err WrongSideToMove
            Some(fPt)
        | None -> 
            err EmptyCell
            None
    
    //   ___________
    //__/ Functions \_____________________________________________________
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
                    if position.EnPassant = Some(toSquare % 16) then enPassant()
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
        let avail opt = position.CastlingAvailability |> contains opt
        
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
        
        let w = IsAttackedBy Black at
        let b = IsAttackedBy White at
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
            else if (at next) <> None then err DoesNotJump
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
    
    let assignMissingPromotionHint() = 
        if observations |> contains Promotion then 
            warnings <- MissingPromotionHint :: warnings
    
    let assignPromotionHintIsNotNeededHint() = 
        if not (observations |> contains Promotion) then 
            warnings <- PromotionHintIsNotNeeded :: warnings
    
    let setupResultPosition() = 
        let newPlacement = Array.copy position.Placement
        // Remove the pawn captured en-passant
        if observations |> contains EnPassant then 
            let increment = 
                if color = White then +8
                else -8
            newPlacement.[(moveTo |> ToIndex) + increment] <- None
        // Remove the piece from the old square and put it to the new square
        let effectivePiece = 
            if observations |> contains Promotion then promoteTo
            else pieceType.Value
        newPlacement.[moveTo |> ToIndex] <- Some((color, effectivePiece))
        newPlacement.[moveFrom |> ToIndex] <- None
        // Move the rook if it was a castling
        let moveCastlingRook f t = 
            let x88toIndex = fromX88 >> ToIndex
            let rook = newPlacement.[fromX88 f |> ToIndex]
            newPlacement.[f |> x88toIndex] <- None
            newPlacement.[t |> x88toIndex] <- rook
        match castling with
        | Some(WK) -> moveCastlingRook H1 F1
        | Some(WQ) -> moveCastlingRook A1 D1
        | Some(BK) -> moveCastlingRook H8 F8
        | Some(BQ) -> moveCastlingRook A8 D8
        | None -> ()
        // Figure out new en-passant option, half-move clock, full-move number
        let newEnPassant = 
            if observations |> contains DoublePush then Some(fst moveFrom)
            else None
        
        let newHalfMoveClock = 
            if pieceType.Value = Pawn || observations |> contains Capture then 0
            else position.HalfMoveClock + 1
        
        let newMoveNumber = 
            position.FullMoveNumber + if color = Black then 1
                                      else 0
        
        // Figure out new castling availability
        let optionsInvalidatedBy p = 
            match p |> toX88 with
            | A1 -> [ WQ ]
            | E1 -> [ WQ; WK ]
            | H1 -> [ WK ]
            | A8 -> [ BQ ]
            | E8 -> [ BQ; BK ]
            | H8 -> [ BK ]
            | _ -> []
        
        let newCastlingAvailability = 
            position.CastlingAvailability
            |> except (optionsInvalidatedBy moveFrom)
            |> except (optionsInvalidatedBy moveTo)
        
        // Figure out new active color, and if the move gives check
        let newActiveColor = Color.oppositeOf color
        { // Construct new position
          position with Placement = newPlacement
                        ActiveColor = newActiveColor
                        EnPassant = newEnPassant
                        HalfMoveClock = newHalfMoveClock
                        FullMoveNumber = newMoveNumber
                        CastlingAvailability = newCastlingAvailability
                        Observations = [] }
    
    //   ______________
    //__/ Calculations \__________________________________________________
    if errors.IsEmpty then 
        validateByPieceType () (toX88 moveFrom) (toX88 moveTo)
    if errors.IsEmpty then 
        let resPos = setupResultPosition()
        resultPosition <- Some(resPos)
        // MoveToCheck
        let at c = PieceAt c resPos
        if IsInCheck (Color.oppositeOf resPos.ActiveColor) at then 
            err MoveToCheck
            resultPosition <- None
        // new position is Check
        let newAt coordinate = resPos.Placement.[coordinate |> ToIndex]
        let isInCheck = IsInCheck resPos.ActiveColor newAt
        
        let newObservations = 
            [ if isInCheck then yield Check ]
        if not newObservations.IsEmpty then 
            resultPosition <- Some
                                  ({ resPos with Observations = newObservations })
    match move with
    | UsualMove(_, _) -> assignMissingPromotionHint()
    | PromotionMove(_) -> assignPromotionHintIsNotNeededHint()
    if errors.IsEmpty then 
        LegalMove { Start = moveFrom
                    End = moveTo
                    PromoteTo = promoteTo
                    OriginalPosition = position
                    ResultPosition = resultPosition.Value
                    Piece = pieceType.Value
                    Castling = castling
                    Observations = observations
                    Warnings = warnings }
    else 
        IllegalMove({ Move = move
                      OriginalPosition = position
                      Piece = pieceType
                      Castling = castling
                      Observations = observations
                      Warnings = warnings
                      Errors = errors })

let UnwrapLegal = 
    function 
    | LegalMove(m) -> m
    | IllegalMove(_) -> failwith "move is illegal"
