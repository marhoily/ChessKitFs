module MoveLegalityChecker

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

[<Literal>]
let A1 = 112

[<Literal>]
let B1 = 113

[<Literal>]
let C1 = 114

[<Literal>]
let D1 = 115

[<Literal>]
let E1 = 116

[<Literal>]
let F1 = 117

[<Literal>]
let G1 = 118

[<Literal>]
let H1 = 119

[<Literal>]
let A8 = 0

[<Literal>]
let B8 = 1

[<Literal>]
let C8 = 2

[<Literal>]
let D8 = 3

[<Literal>]
let E8 = 4

[<Literal>]
let F8 = 5

[<Literal>]
let G8 = 6

[<Literal>]
let H8 = 7

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
    let mutable piece = None
    let mutable resultPosition = None
    let doesNotCaptureThisWay() = errors <- DoesNotCaptureThisWay :: errors
    let doesNotMoveThisWay() = errors <- DoesNotMoveThisWay :: errors
    let doesNotJump() = errors <- DoesNotJump :: errors
    let promotion() = observations <- Promotion :: observations
    let doublePush() = observations <- DoublePush :: observations
    let onlyCapturesThisWay() = errors <- OnlyCapturesThisWay :: errors
    let enPassant() = observations <- Capture :: EnPassant :: observations
    let hasNoCastling() = errors <- HasNoCastling :: errors
    let castleThroughCheck() = errors <- CastleThroughCheck :: errors
    let castleFromCheck() = errors <- CastleFromCheck :: errors
    let toOccupiedCell() = errors <- ToOccupiedCell :: errors
    let wrongSideToMove() = errors <- WrongSideToMove :: errors
    let capture() = observations <- Capture :: observations
    let emptyCell() = errors <- EmptyCell :: errors
    let castle x = castling <- Some(x)
    
    let hasNoEnPassant() = 
        errors <- HasNoEnPassant :: errors
        observations <- Capture :: EnPassant :: observations
    
    let f2, t2, p2 = 
        match move with
        | UsualMove(f, t) -> (f, t, Queen)
        | PromotionMove({ Vector = (f, t); PromoteTo = p }) -> (f, t, p)
    let at64 i64 = position |> PieceAt i64
    let fP2 = at64 f2
    let capturedPiece = at64 t2
    let at i = position |> PieceAt(i % 16, i / 16)
    
    let validatePawnMove sideToMove fromSquare toSquare = 
        let validateDoublePush v c = 
            if fromSquare / 16 <> c then doesNotMoveThisWay()
            else if at toSquare <> None then doesNotCaptureThisWay()
            else if at (fromSquare + v) <> None then doesNotJump()
            else doublePush()
        
        let validatePush c = 
            if at toSquare <> None then doesNotCaptureThisWay()
            else if fromSquare / 16 = c then promotion()
        
        let validateCapture c2 looksEnPassanty = 
            if at toSquare = None then 
                if looksEnPassanty() then 
                    if position.EnPassant = Some(toSquare % 16) then enPassant()
                    else hasNoEnPassant()
                else onlyCapturesThisWay()
            else if fromSquare / 16 = c2 then promotion()
        
        let looksEnPassanty c1 c2 c3 color () = 
            fromSquare / 16 = c1 && at (fromSquare + c2) = None 
            && at (fromSquare + c3) = (Some(color, Pawn))
        match (sideToMove, (toSquare - fromSquare)) with
        | (White, -32) -> validateDoublePush -16 6
        | (Black, +32) -> validateDoublePush +16 1
        | (White, -16) -> validatePush 1
        | (Black, +16) -> validatePush 6
        | (White, -15) -> validateCapture 1 (looksEnPassanty 3 -31 +1 Black)
        | (White, -17) -> validateCapture 1 (looksEnPassanty 3 -33 -1 Black)
        | (Black, +17) -> validateCapture 6 (looksEnPassanty 4 +33 +1 White)
        | (Black, +15) -> validateCapture 6 (looksEnPassanty 4 +31 -1 White)
        | _ -> doesNotMoveThisWay()
    
    let validateKnightMove f t = 
        match (t - f) with
        | 33 | 31 | -33 | -31 | 18 | 14 | -18 | -14 -> ()
        | _ -> doesNotMoveThisWay()
    
    let validateKingMove fromSquare toSquare = 
        let avail opt = position.CastlingAvailability |> contains opt
        
        let long B C D E attacked castlingOpt = 
            if at D <> None || at B <> None then doesNotJump()
            else if at C <> None then doesNotCaptureThisWay()
            else if not (avail castlingOpt) then hasNoCastling()
            else if attacked E then castleFromCheck()
            else if attacked D then castleThroughCheck()
            castle (castlingOpt)
        
        let short E F G attacked castlingOpt = 
            if at F <> None then doesNotJump()
            else if at G <> None then doesNotCaptureThisWay()
            else if not (avail castlingOpt) then hasNoCastling()
            else if attacked E then castleFromCheck()
            else if attacked F then castleThroughCheck()
            castle (castlingOpt)
        
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
            | _ -> doesNotMoveThisWay()
        | _ -> doesNotMoveThisWay()
    
    let validateSlidingMove offsets f t = 
        let rec iterate start stop increment = 
            let next = start + increment
            if next &&& 0x88 <> 0 then doesNotMoveThisWay()
            else if next = stop then ()
            else if (at next) <> None then doesNotJump()
            else iterate next stop increment
        
        let isMultipleOf n m = n % m = 0 && n / m < 8 && n / m >= 0
        match offsets |> Seq.tryFind (t - f |> isMultipleOf) with
        | Some(m) -> iterate f t m
        | None -> doesNotMoveThisWay()
    
    let validateBishopMove = validateSlidingMove [ 15; -15; 17; -17 ]
    let validateRookMove = validateSlidingMove [ 16; -16; 01; -01 ]
    let validateQueenMove = 
        validateSlidingMove [ 16; -16; 01; -01; 15; -15; 17; -17 ]
    
    let validateByPieceType sideToMove pieceType = 
        match pieceType with
        | Pawn -> validatePawnMove sideToMove
        | Knight -> validateKnightMove
        | King -> validateKingMove
        | Bishop -> validateBishopMove
        | Rook -> validateRookMove
        | Queen -> validateQueenMove
    
    let addPieceType pieceType = piece <- Some(pieceType)
    
    if capturedPiece <> None then 
        if (fst capturedPiece.Value) = position.ActiveColor then toOccupiedCell()
        else capture()
    
    let checkSideToMove color = 
        if position.ActiveColor <> color then wrongSideToMove()
    
    let assignMoveToCheckError() = 
        match resultPosition with
        | Some(p) -> 
            let at c = PieceAt c p
            if IsInCheck (Color.oppositeOf p.ActiveColor) at then 
                errors <- MoveToCheck :: errors
                resultPosition <- None
        | None -> ()
    
    let assignMissingPromotionHint() = 
        if observations |> contains Promotion then 
            warnings <- MissingPromotionHint :: warnings
    
    let assignPromotionHintIsNotNeededHint() = 
        if not (observations |> contains Promotion) then 
            warnings <- PromotionHintIsNotNeeded :: warnings
    
    let addObservations() = 
        match resultPosition with
        | Some(p) -> 
            let newAt coordinate = p.Placement.[coordinate |> ToIndex]
            let isInCheck = IsInCheck p.ActiveColor newAt
            
            let newObservations = 
                [ if isInCheck then yield Check ]
            if not newObservations.IsEmpty then 
                resultPosition <- Some
                                      ({ p with Observations = newObservations })
        | None -> ()
    
    let setupResultPosition fPt color = 
        let newPlacement = Array.copy position.Placement
        // Remove the pawn captured en-passant
        if observations |> contains EnPassant then 
            let increment = 
                if color = White then +8
                else -8
            newPlacement.[(t2 |> ToIndex) + increment] <- None
        // Remove the piece from the old square and put it to the new square
        let piece1 = 
            if observations |> contains Promotion then p2
            else fPt
        newPlacement.[t2 |> ToIndex] <- Some((color, piece1))
        newPlacement.[f2 |> ToIndex] <- None
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
            if observations |> contains DoublePush then Some(fst f2)
            else None
        
        let newHalfMoveClock = 
            if piece = Some(Pawn) || observations |> contains Capture then 0
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
            |> except (optionsInvalidatedBy f2)
            |> except (optionsInvalidatedBy t2)
        
        // Figure out new active color, and if the move gives check
        let newActiveColor = Color.oppositeOf position.ActiveColor
        { // Construct new position
          position with Placement = newPlacement
                        ActiveColor = newActiveColor
                        EnPassant = newEnPassant
                        HalfMoveClock = newHalfMoveClock
                        FullMoveNumber = newMoveNumber
                        CastlingAvailability = newCastlingAvailability
                        Observations = [] }
    
    let assignResultPosition fPt color = 
        if errors.IsEmpty then 
            let p = Some(setupResultPosition fPt color)
            resultPosition <- p
    
    let validateFromTo() = 
        match fP2 with
        | Some(color, fPt) -> 
            validateByPieceType color fPt (toX88 f2) (toX88 t2)
            addPieceType fPt
           // checkCapture capturedPiece
            checkSideToMove color
            assignResultPosition fPt color
            assignMoveToCheckError()
            addObservations()
        | None -> emptyCell()

    match move with
    | UsualMove(_, _) -> 
        validateFromTo()
        assignMissingPromotionHint()
    | PromotionMove(_) -> 
        validateFromTo()
        assignPromotionHintIsNotNeededHint()

    if errors.IsEmpty then 
        LegalMove { Start = f2
                    End = t2
                    PromoteTo = p2
                    OriginalPosition = position
                    ResultPosition = resultPosition.Value
                    Piece = piece.Value
                    Castling = castling
                    Observations = observations
                    Warnings = warnings }
    else 
        IllegalMove({ Move = move
                      OriginalPosition = position
                      Piece = piece
                      Castling = castling
                      Observations = observations
                      Warnings = warnings
                      Errors = errors })

let UnwrapLegal = 
    function 
    | LegalMove(m) -> m
    | IllegalMove(_) -> failwith "move is illegal"
