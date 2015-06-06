module MoveLegalityChecker

open CoordinateNotation
open Definitions
open IsAttackedBy
open MyList

type ObservationHint = 
    | Capture
    | EnPassant
    | Promotion
    | DoublePush

type WarningHint = 
    | MissingPromotionHint
    | PromotionHintIsNotNeeded

type ErrorHint = 
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

type private Hint = 
    { Piece : PieceType option
      Castling : CastlingHint option
      Observations : ObservationHint list
      Errors : ErrorHint list
      Warnings : WarningHint list
      ResultPosition : Position option }

let private eh = 
    { Piece = None
      Castling = None
      Observations = []
      Errors = []
      Warnings = []
      ResultPosition = None }

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
      Observations : ObservationHint list
      Warnings : WarningHint list }

type IllegalMove = 
    { Move : Move
      OriginalPosition : Position
      Piece : PieceType option
      Castling : CastlingHint option
      Observations : ObservationHint list
      Warnings : WarningHint list
      Errors : ErrorHint list }

type MoveInfo = 
    | LegalMove of LegalMove
    | IllegalMove of IllegalMove

let ValidateMove move position = 
    let doesNotCaptureThisWay = { eh with Errors = [ DoesNotCaptureThisWay ] }
    let doesNotMoveThisWay = { eh with Errors = [ DoesNotMoveThisWay ] }
    let doesNotJump = { eh with Errors = [ DoesNotJump ] }
    let promotion = { eh with Observations = [ Promotion ] }
    let doublePush = { eh with Observations = [ DoublePush ] }
    let onlyCapturesThisWay = { eh with Errors = [ OnlyCapturesThisWay ] }
    let enPassant = { eh with Observations = [ Capture; EnPassant ] }
    let hasNoCastling = { eh with Errors = [ HasNoCastling ] }
    let castleThroughCheck = { eh with Errors = [ CastleThroughCheck ] }
    let castleFromCheck = { eh with Errors = [ CastleFromCheck ] }
    
    let hasNoEnPassant = 
        { eh with Observations = [ Capture; EnPassant ]
                  Errors = [ HasNoEnPassant ] }
    
    let at i = position |> PieceAt(i % 16, i / 16)
    
    let validatePawnMove sideToMove fromSquare toSquare = 
        let validateDoublePush v c = 
            if fromSquare / 16 <> c then doesNotMoveThisWay
            else if at toSquare <> None then doesNotCaptureThisWay
            else if at (fromSquare + v) <> None then doesNotJump
            else doublePush
        
        let validatePush c = 
            if at toSquare <> None then doesNotCaptureThisWay
            else if fromSquare / 16 = c then promotion
            else eh
        
        let validateCapture c2 looksEnPassanty = 
            if at toSquare = None then 
                if looksEnPassanty() then 
                    if position.EnPassant = Some(toSquare % 16) then enPassant
                    else hasNoEnPassant
                else onlyCapturesThisWay
            else if fromSquare / 16 = c2 then promotion
            else eh
        
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
        | _ -> doesNotMoveThisWay
    
    let validateKnightMove f t = 
        match (t - f) with
        | 33 | 31 | -33 | -31 | 18 | 14 | -18 | -14 -> eh
        | _ -> doesNotMoveThisWay
    
    let validateKingMove fromSquare toSquare = 
        let castling opt = position.CastlingAvailability |> contains opt
        
        let long B C D E attacked available = 
            if at D <> None || at B <> None then doesNotJump
            else if at C <> None then doesNotCaptureThisWay
            else if not (castling available) then hasNoCastling
            else if attacked E then castleFromCheck
            else if attacked D then castleThroughCheck
            else eh
        
        let short E F G attacked available = 
            if at F <> None then doesNotJump
            else if at G <> None then doesNotCaptureThisWay
            else if not (castling available) then hasNoCastling
            else if attacked E then castleFromCheck
            else if attacked F then castleThroughCheck
            else eh
        
        let w = IsAttackedBy Black at
        let b = IsAttackedBy White at
        match (toSquare - fromSquare) with
        | 1 | 15 | 16 | 17 | -1 | -15 | -16 | -17 -> eh
        | -2 | +2 -> 
            match (fromSquare, toSquare) with
            | (E1, C1) -> { long B1 C1 D1 E1 w WQ with Castling = Some(WQ) }
            | (E8, C8) -> { long B8 C8 D8 E8 b BQ with Castling = Some(BQ) }
            | (E1, G1) -> { short E1 F1 G1 w WK with Castling = Some(WK) }
            | (E8, G8) -> { short E8 F8 G8 b BK with Castling = Some(BK) }
            | _ -> doesNotMoveThisWay
        | _ -> doesNotMoveThisWay
    
    let validateSlidingMove offsets f t = 
        let rec iterate start stop increment = 
            let next = start + increment
            if next &&& 0x88 <> 0 then doesNotMoveThisWay
            else if next = stop then eh
            else if (at next) <> None then doesNotJump
            else iterate next stop increment
        
        let isMultipleOf n m = n % m = 0 && n / m < 8 && n / m >= 0
        match offsets |> Seq.tryFind (t - f |> isMultipleOf) with
        | Some(m) -> iterate f t m
        | None -> doesNotMoveThisWay
    
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
    
    let addPieceType pieceType (hint : Hint) = 
        { hint with Piece = Some(pieceType) }
    
    let checkCapture capture (hint : Hint) = 
        if capture <> None then 
            if (fst capture.Value) = position.ActiveColor then 
                { hint with Errors = ToOccupiedCell :: hint.Errors }
            else { hint with Observations = Capture :: hint.Observations }
        else hint
    
    let checkSideToMove color (hint : Hint) = 
        if position.ActiveColor <> color then 
            { hint with Errors = WrongSideToMove :: hint.Errors }
        else hint
    
    let assignMoveToCheckError (hint : Hint) = 
        match hint.ResultPosition with
        | Some(p) -> 
            let at c = PieceAt c p
            if IsInCheck (Color.oppositeOf p.ActiveColor) at then 
                { hint with Errors = MoveToCheck :: hint.Errors
                            ResultPosition = None }
            else hint
        | None -> hint
    
    let assignMissingPromotionHint (hint : Hint) = 
        if hint.Observations |> contains Promotion then 
            { hint with Warnings = MissingPromotionHint :: hint.Warnings }
        else hint
    
    let assignPromotionHintIsNotNeededHint (hint : Hint) = 
        if not (hint.Observations |> contains Promotion) then 
            { hint with Warnings = PromotionHintIsNotNeeded :: hint.Warnings }
        else hint
    
    let addObservations (hint : Hint) = 
        match hint.ResultPosition with
        | Some(p) -> 
            let newAt coordinate = p.Placement.[coordinate |> ToIndex]
            let isInCheck = IsInCheck p.ActiveColor newAt
            
            let newObservations = 
                [ if isInCheck then yield Check ]
            if not newObservations.IsEmpty then 
                { hint with ResultPosition = 
                                Some({ p with Observations = newObservations }) }
            else hint
        | None -> hint
    
    let setupResultPosition f t promoteTo fPt color (hint : Hint) = 
        let newPlacement = Array.copy position.Placement
        // Remove the pawn captured en-passant
        if hint.Observations |> contains EnPassant then 
            let capture = 
                if color = White then +8
                else -8
            newPlacement.[(t |> ToIndex) + capture] <- None
        // Remove the piece from the old square and put it to the new square
        let piece = 
            if hint.Observations |> contains Promotion then promoteTo
            else fPt
        newPlacement.[t |> ToIndex] <- Some((color, piece))
        newPlacement.[f |> ToIndex] <- None
        // Move the rook if it was a castling
        let moveCastlingRook f t = 
            let x88toIndex = fromX88 >> ToIndex
            let rook = newPlacement.[fromX88 f |> ToIndex]
            newPlacement.[f |> x88toIndex] <- None
            newPlacement.[t |> x88toIndex] <- rook
        match hint.Castling with
        | Some(WK) -> moveCastlingRook H1 F1
        | Some(WQ) -> moveCastlingRook A1 D1
        | Some(BK) -> moveCastlingRook H8 F8
        | Some(BQ) -> moveCastlingRook A8 D8
        | None -> ()
        // Figure out new en-passant option, half-move clock, full-move number
        let newEnPassant = 
            if hint.Observations |> contains DoublePush then Some(fst f)
            else None
        
        let newHalfMoveClock = 
            if hint.Piece = Some(Pawn) || hint.Observations |> contains Capture then 
                0
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
            |> except (optionsInvalidatedBy f)
            |> except (optionsInvalidatedBy t)
        
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
    
    let assignResultPosition f t promoteTo fPt color (hint : Hint) = 
        if hint.Errors.IsEmpty then 
            let p = Some(setupResultPosition f t promoteTo fPt color hint)
            { hint with ResultPosition = p }
        else hint
    
    let at64 i64 = position |> PieceAt i64
    
    let validateFromTo f t promoteTo = 
        match at64 f with
        | Some(color, fPt) -> 
            validateByPieceType color fPt (toX88 f) (toX88 t)
            |> addPieceType fPt
            |> checkCapture (at64 t)
            |> checkSideToMove color
            |> assignResultPosition f t promoteTo fPt color
            |> assignMoveToCheckError
            |> addObservations
        | None -> { eh with Errors = [ ErrorHint.EmptyCell ] }
    
    let hint = 
        match move with
        | UsualMove(f, t) -> 
            validateFromTo f t Queen |> assignMissingPromotionHint
        | PromotionMove({ Vector = (f, t); PromoteTo = promoteTo }) -> 
            validateFromTo f t promoteTo |> assignPromotionHintIsNotNeededHint
    
    if hint.Errors |> List.isEmpty then 
        let (f, t, p) = 
            match move with
            | UsualMove(f, t) -> (f, t, Queen)
            | PromotionMove({ Vector = (f, t); PromoteTo = p }) -> (f, t, p)
        LegalMove { Start = f
                    End = t
                    PromoteTo = p
                    OriginalPosition = position
                    ResultPosition = hint.ResultPosition.Value
                    Piece = hint.Piece.Value
                    Castling = hint.Castling
                    Observations = hint.Observations
                    Warnings = hint.Warnings }
    else 
        IllegalMove({ Move = move
                      OriginalPosition = position
                      Piece = hint.Piece
                      Castling = hint.Castling
                      Observations = hint.Observations
                      Warnings = hint.Warnings
                      Errors = hint.Errors })

let UnwrapLegal = 
    function 
    | LegalMove(m) -> m
    | IllegalMove(_) -> failwith "move is illegal"
